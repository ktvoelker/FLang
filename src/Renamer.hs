
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
      lift2 getAccum >>= lift3 . report . EInternal . show
      return result

allocUnique :: (MonadState Global m) => m Integer
allocUnique = gNextUnique %= (+ 1)

insertRef :: (Monad m) => Integer -> AR m ()
insertRef n = getAccum >>= putAccum . Set.insert n

class (Decl a, Show a) => RenameDecl a where
  renameDecl :: a -> M a

class RenamePrim a where
  renamePrim :: a -> M a

instance RenameDecl ModDecl where
  renameDecl (BindMod b) = BindMod <$> renameBinding b
  renameDecl (BindSig b) = BindSig <$> renameBinding b
  renameDecl (BindVal b) = BindVal <$> renameBinding b
  renameDecl (BindTy b) = BindTy <$> renameBinding b
  renameDecl (Data m n p t ds) = do
    n'  <- renameNameBind n
    p'  <- maybe (return Nothing) (fmap Just . renameExpr) p
    t'  <- renameExpr t
    ds' <- mapM renameDecl ds
    return $ Data m n' p' t' ds'
  renameDecl (Infix a p ns) = Infix a p <$> mapM renameNameRef ns

instance RenameDecl SigDecl where
  renameDecl (SigVal n e) = SigVal <$> renameNameBind n <*> renameExpr e
  renameDecl (SigTy n ty) = SigTy <$> renameNameBind n <*> ty'
    where
      ty' = case ty of
        Nothing -> pure Nothing
        Just (TyBound o e) -> Just . TyBound o <$> renameExpr e
  renameDecl (SigMod n e) = SigMod <$> renameNameBind n <*> renameExpr e

instance RenameDecl ValDecl where
  renameDecl (BindLocalVal b) = BindLocalVal <$> renameBinding b

instance RenameDecl TyDecl where
  renameDecl (FieldDecl n e) = FieldDecl <$> renameNameBind n <*> renameExpr e
  renameDecl (Constraint a o b) =
    Constraint <$> renameExpr a <*> pure o <*> renameExpr b

renameBinding
  :: (RenameDecl d, RenamePrim e) => Binding (Expr d e) -> M (Binding (Expr d e))
renameBinding (Binding (Binder n t) e) = do
  n' <- renameNameBind n
  t' <- maybe (return Nothing) (fmap Just . renameExpr) t
  Binding (Binder n' t') <$> renameExpr e

renameNameFrom :: Lens Env (Map BindName Integer) -> BindName -> M BindName
renameNameFrom field n@(BindName xs) = do
  env <- ask
  let z = Map.lookup n $ env ^. field
  case z of
    Nothing -> do
      lift3 . report . EUnbound $ xs
      return n
    Just z -> return $ UniqueName z xs
renameNameFrom _ n@(UniqueName _ _) = return n

renameNameRef :: BindName -> M BindName
renameNameRef n = do
  n' <- renameNameFrom eScope n
  case n' of
    UniqueName z _ -> lift2 . insertRef $ z
    _ -> return ()
  return n'

renameNameBind :: BindName -> M BindName
renameNameBind = renameNameFrom eBinds

renameBinders :: [Binder] -> M [Binder]
renameBinders = mapM renameBinder

renameBinder :: Binder -> M Binder
renameBinder (Binder name ty) = Binder <$> renameNameBind name <*> mapM renameExpr ty

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

renameSortDecls :: (RenameDecl d) => [d] -> M [d]
renameSortDecls ds = mapM (branch . renameDecl) ds >>= lift3 . sortDecls

renameExpr :: (RenameDecl d, RenamePrim e) => Expr d e -> M (Expr d e)
renameExpr (Lam bs e) = do
  env' <- makeBindEnv bs
  local (const env') $ Lam <$> renameBinders bs <*> withBindsInScope (renameExpr e)
renameExpr (App f as) = App <$> renameExpr f <*> mapM renameExpr as
renameExpr (Record ds) = do
  env' <- makeRecEnv ds
  Record <$> local (const env') (renameSortDecls ds)
renameExpr (Ref n) = Ref <$> renameNameRef n
renameExpr e@(UniqueRef _) = return e
renameExpr (Member e n) = Member <$> renameExpr e <*> pure n
renameExpr (OpChain e os) = OpChain <$> mapM renameExpr e <*> mapM f os
  where
    f (a, b) = (,) <$> renameExpr a <*> renameExpr b
renameExpr (Let ds e) = do
  env' <- makeRecEnv ds
  ds' <- local (const env') $ renameSortDecls ds
  e' <- local (const env') $ renameExpr e
  Let <$> pure ds' <*> pure e'
renameExpr (Prim p) = Prim <$> renamePrim p
renameExpr ToDo = return ToDo

instance RenamePrim No where
  renamePrim no = do
    lift3 . report $ EInternal "Unexpected Prim found in ModExpr or SigExpr"
    return no

instance RenamePrim ValPrim where
  renamePrim (LamCase xs) = LamCase <$> mapM renameCaseClause xs
  renamePrim (Case e xs) = Case <$> renameExpr e <*> mapM renameCaseClause xs
  renamePrim (Do xs) = Do <$> renameDo xs
  renamePrim lit = return lit

instance RenamePrim TyPrim where
  renamePrim = return

renameCaseClause (CaseClause p v) = do
  (p', env') <- renamePat p
  CaseClause p' <$> local (const env') (withBindsInScope $ renameExpr v)

renamePat :: Pat -> M (Pat, Env)
renamePat pat = do
  env' <- makeBindEnv [pat]
  (, env') <$> local (const env') (renamePat' pat)

renamePat' :: Pat -> M Pat
renamePat' (PatParams ps) = PatParams <$> mapM renamePat' ps
renamePat' (PatApp e ps) = PatApp <$> renameExpr e <*> mapM renamePat' ps
renamePat' (PatBind b) = PatBind <$> renameNameBind b
renamePat' lit = return lit

renameDo [] = return []
renameDo (DoLet ds : xs) = do
  -- TODO we need to do renaming on the decls
  env' <- makeRecEnv ds
  (DoLet ds :) <$> local (const env') (renameDo xs)
renameDo (DoBind p v : xs) = do
  (p', env') <- renamePat p
  (:) <$> (DoBind p' <$> renameExpr v) <*> local (const env') (renameDo xs)
renameDo (DoExpr e : xs) = (:) <$> (DoExpr <$> renameExpr e) <*> renameDo xs

