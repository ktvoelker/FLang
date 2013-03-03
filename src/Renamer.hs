
module Renamer where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Common
import Syntax
import Types

import Renamer.Sorter

type AR = AccumT (Set Integer)

type M = ReaderT Env (StateT Global (AR FM))

rename :: Program -> FM Global
rename p =
  evalAccumT (execStateT (runReaderT f emptyEnv) $ emptyGlobal $ p) Set.empty Set.union
  where
    f = do
      result <- gRoot %>>= renameExpr
      lift2 getAccum >>= lift3 . internal
      return result

allocUnique :: (MonadState Global m) => m Integer
allocUnique = gNextUnique %= (+ 1)

insertRef :: (Monad m) => Integer -> AR m ()
insertRef n = getAccum >>= putAccum . Set.insert n

class (Decl a) => RenameDecl a where
  renameDecl :: a -> M a

class RenamePrim a where
  renamePrim :: a -> M a

class
  ( RenameDecl (ExprDecl k)
  , RenamePrim (ExprPrim k)
  , RenameDecl (ExprDecl (ExprTy k))
  , RenamePrim (ExprPrim (ExprTy k))
  ) => RenameExpr k where

instance RenameExpr ModK where

instance RenameExpr ValK where

instance RenameExpr TyK where

instance RenameExpr KindK where

instance RenameExpr NoK where

instance RenameDecl No where
  renameDecl _ = undefined

instance RenameDecl ModDecl where
  renameDecl (BindMod a b) = BindMod a <$> renameBinding b
  renameDecl (BindSig a b) = BindSig a <$> renameBinding b
  renameDecl (BindVal a b) = BindVal a <$> renameBinding b
  renameDecl (BindTy a b) = BindTy a <$> renameBinding b
  renameDecl (Data a m n p t ds) = do
    n'  <- renameNameBind n
    p'  <- maybe (return Nothing) (fmap Just . renameExpr) p
    t'  <- renameExpr t
    ds' <- mapM renameDecl ds
    return $ Data a m n' p' t' ds'
  renameDecl (Infix ann a p ns) = Infix ann a p <$> mapM renameNameRef ns

instance RenameDecl ValDecl where
  renameDecl (BindLocalVal a b) = BindLocalVal a <$> renameBinding b

instance RenameDecl TyDecl where
  renameDecl (Constraint ann a o b) =
    Constraint ann <$> renameExpr a <*> pure o <*> renameExpr b
  renameDecl (ValField a n e) = ValField a <$> renameNameBind n <*> renameExpr e
  renameDecl (TyField a n ty) = TyField a <$> renameNameBind n <*> ty'
    where
      ty' = case ty of
        Nothing -> pure Nothing
        Just (TyBound a o e) -> Just . TyBound a o <$> renameExpr e
  renameDecl (ModField a n e) = ModField a <$> renameNameBind n <*> renameExpr e

renameBinding :: (RenameExpr k) => Binding k -> M (Binding k)
renameBinding (Binding (Binder n t) e) = do
  n' <- renameNameBind n
  t' <- maybe (return Nothing) (fmap Just . renameExpr) t
  Binding (Binder n' t') <$> renameExpr e

renameNameFrom :: Lens Env (Map BindName Integer) -> BindName -> M BindName
renameNameFrom field n@(BindName a xs) = do
  env <- ask
  let z = Map.lookup n $ env ^. field
  case z of
    Nothing -> do
      lift3
        . report
        $ Err EUnbound (a ^. annSourcePos) (Just n) Nothing
      return n
    Just z -> return $ UniqueName a z xs
renameNameFrom _ n@(UniqueName _ _ _) = return n

renameNameRef :: BindName -> M BindName
renameNameRef n = do
  n' <- renameNameFrom eScope n
  case n' of
    UniqueName _ z _ -> lift2 . insertRef $ z
    _ -> return ()
  return n'

renameNameBind :: BindName -> M BindName
renameNameBind = renameNameFrom eBinds

renameBinders
  :: ( RenameDecl (ExprDecl k)
     , RenamePrim (ExprPrim k)
     , RenameDecl (ExprDecl (ExprTy k))
     , RenamePrim (ExprPrim (ExprTy k))
     ) => [Binder k] -> M [Binder k]
renameBinders = mapM renameBinder

renameBinder
  :: ( RenameDecl (ExprDecl k)
     , RenamePrim (ExprPrim k)
     , RenameDecl (ExprDecl (ExprTy k))
     , RenamePrim (ExprPrim (ExprTy k))
     ) => Binder k -> M (Binder k)
renameBinder (Binder name ty) =
  Binder <$> renameNameBind name <*> mapM renameExpr ty

makeEnv :: (HasBindNames a) => Bool -> [a] -> M Env
makeEnv inScope ds = do
  env <- ask
  newPairs <- mapM (\b -> (b, ) <$> allocUnique) $ ds >>= bindNames
  let f = (^%= Map.union (Map.fromList newPairs))
  return $ f eBinds (if inScope then f eScope env else env)

makeRecEnv :: (HasBindNames a) => [a] -> M Env
makeRecEnv = makeEnv True

makeBindEnv :: (HasBindNames a) => [a] -> M Env
makeBindEnv = makeEnv False

renameSortDecls
  :: (RenameDecl (ExprDecl k))
  => ExprTag k -> [ExprDecl k] -> M [ExprDecl k]
renameSortDecls _ ds = mapM (branch . renameDecl) ds >>= lift3 . sortDecls

renameExpr :: (RenameExpr k) => Expr k -> M (Expr k)
renameExpr (Lam a bs e) = do
  env' <- makeBindEnv bs
  local (const env') $ Lam a <$> renameBinders bs <*> withBindsInScope (renameExpr e)
renameExpr (App a f as) = App a <$> renameExpr f <*> mapM renameExpr as
renameExpr e@(Record a ds) = do
  env' <- makeRecEnv ds
  Record a <$> local (const env') (renameSortDecls (exprTag e) ds)
renameExpr (Ref a n) = Ref a <$> renameNameRef n
renameExpr e@(UniqueRef _ _) = return e
renameExpr (Member a e n) = Member a <$> renameExpr e <*> pure n
renameExpr (OpChain a e os) = OpChain a <$> mapM renameExpr e <*> mapM f os
  where
    f (a, b) = (,) <$> renameExpr a <*> renameExpr b
renameExpr (Let a ds e) = do
  env' <- makeRecEnv ds
  ds' <- local (const env') $ renameSortDecls (exprTag e) ds
  e' <- local (const env') $ renameExpr e
  Let a <$> pure ds' <*> pure e'
renameExpr (Prim a p) = Prim a <$> renamePrim p
renameExpr e@(ToDo _) = return e

instance RenamePrim No where
  renamePrim no = do
    lift3 . internal $ "Unexpected Prim found in ModExpr or SigExpr"
    return no

-- TODO
instance RenamePrim KindPrim where
  renamePrim _ = undefined

instance RenamePrim ValPrim where
  renamePrim (LamCase a xs) = LamCase a <$> mapM renameCaseClause xs
  renamePrim (Case a e xs) = Case a <$> renameExpr e <*> mapM renameCaseClause xs
  renamePrim (Do a xs) = Do a <$> renameDo xs
  renamePrim lit = return lit

instance RenamePrim TyPrim where
  renamePrim = return

renameCaseClause (CaseClause a p v) = do
  (p', env') <- renamePat p
  CaseClause a p' <$> local (const env') (withBindsInScope $ renameExpr v)

renamePat :: Pat -> M (Pat, Env)
renamePat pat = do
  env' <- makeBindEnv [pat]
  (, env') <$> local (const env') (renamePat' pat)

renamePat' :: Pat -> M Pat
renamePat' (PatParams a ps) = PatParams a <$> mapM renamePat' ps
renamePat' (PatApp a e ps) = PatApp a <$> renameExpr e <*> mapM renamePat' ps
renamePat' (PatBind a b) = PatBind a <$> renameNameBind b
renamePat' lit = return lit

renameDo [] = return []
renameDo (DoLet a ds : xs) = do
  -- TODO we need to do renaming on the decls
  env' <- makeRecEnv ds
  (DoLet a ds :) <$> local (const env') (renameDo xs)
renameDo (DoBind a p v : xs) = do
  (p', env') <- renamePat p
  (:) <$> (DoBind a p' <$> renameExpr v) <*> local (const env') (renameDo xs)
renameDo (DoExpr a e : xs) = (:) <$> (DoExpr a <$> renameExpr e) <*> renameDo xs

