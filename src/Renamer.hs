
module Renamer where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Common
import Syntax
import Types

import Renamer.Sorter

instance LocEnv Env where
  locLens = eLoc

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
    n'  <- keepLoc renameNameBind n
    p'  <- maybe (return Nothing) (fmap Just . keepLoc renameExpr) p
    t'  <- keepLoc renameExpr t
    ds' <- mapM (keepLoc renameDecl) ds
    return $ Data m n' p' t' ds'
  renameDecl (Infix a p ns) = Infix a p <$> mapM (keepLoc renameNameRef) ns

instance RenameDecl SigDecl where
  renameDecl (SigVal n e) = SigVal <$> keepLoc renameNameBind n <*> keepLoc renameExpr e
  renameDecl (SigTy n ty) = SigTy <$> keepLoc renameNameBind n <*> ty'
    where
      ty' = case ty of
        Nothing -> pure Nothing
        Just (L (TyBound o e) loc) ->
          Just . flip L loc . TyBound o <$> keepLoc renameExpr e
  renameDecl (SigMod n e) = SigMod <$> keepLoc renameNameBind n <*> keepLoc renameExpr e

instance RenameDecl ValDecl where
  renameDecl (BindLocalVal b) = BindLocalVal <$> renameBinding b

instance RenameDecl TyDecl where
  renameDecl (FieldDecl n e) =
    FieldDecl <$> keepLoc renameNameBind n <*> keepLoc renameExpr e
  renameDecl (Constraint a o b) =
    Constraint <$> keepLoc renameExpr a <*> pure o <*> keepLoc renameExpr b

renameBinding
  :: (RenameDecl d, RenamePrim e) => Binding (Expr d e) -> M (Binding (Expr d e))
renameBinding (Binding (Binder n t) e) = do
  n' <- keepLoc renameNameBind n
  t' <- maybe (return Nothing) (fmap Just . keepLoc renameExpr) t
  Binding (Binder n' t') <$> keepLoc renameExpr e

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

renameBinders :: [L Binder] -> M [L Binder]
renameBinders = mapM $ keepLoc renameBinder

renameBinder :: Binder -> M Binder
renameBinder (Binder name ty) =
  Binder <$> keepLoc renameNameBind name <*> mapM (keepLoc renameExpr) ty

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

renameSortDecls :: (RenameDecl d) => [L d] -> M [L d]
renameSortDecls ds = mapM (branch . keepLoc renameDecl) ds >>= lift3 . sortDecls

renameExpr :: (RenameDecl d, RenamePrim e) => Expr d e -> M (Expr d e)
renameExpr (Lam bs e) = do
  env' <- makeBindEnv bs
  local (const env')
    $ Lam <$> renameBinders bs <*> withBindsInScope (keepLoc renameExpr e)
renameExpr (App f as) = App <$> keepLoc renameExpr f <*> mapM (keepLoc renameExpr) as
renameExpr (Record ds) = do
  env' <- makeRecEnv ds
  Record <$> local (const env') (renameSortDecls ds)
renameExpr (Ref n) = Ref <$> keepLoc renameNameRef n
renameExpr e@(UniqueRef _) = return e
renameExpr (Member e n) = Member <$> keepLoc renameExpr e <*> pure n
renameExpr (OpChain e os) = OpChain <$> mapM (keepLoc renameExpr) e <*> mapM f os
  where
    f (a, b) = (,) <$> keepLoc renameExpr a <*> keepLoc renameExpr b
renameExpr (Let ds e) = do
  env' <- makeRecEnv ds
  ds' <- local (const env') $ renameSortDecls ds
  e' <- local (const env') $ keepLoc renameExpr e
  Let <$> pure ds' <*> pure e'
renameExpr (Prim p) = Prim <$> renamePrim p
renameExpr ToDo = return ToDo

instance RenamePrim No where
  renamePrim no = do
    lift3 . report $ EInternal "Unexpected Prim found in ModExpr or SigExpr"
    return no

instance RenamePrim ValPrim where
  renamePrim (LamCase xs) = LamCase <$> mapM (keepLoc renameCaseClause) xs
  renamePrim (Case e xs) =
    Case <$> keepLoc renameExpr e <*> mapM (keepLoc renameCaseClause) xs
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
renamePat' (PatParams ps) = PatParams <$> mapM (keepLoc renamePat') ps
renamePat' (PatApp e ps) =
  PatApp <$> keepLoc renameExpr e <*> mapM (keepLoc renamePat') ps
renamePat' (PatBind b) = PatBind <$> keepLoc renameNameBind b
renamePat' lit = return lit

renameDo [] = return []
renameDo (L (DoLet ds) loc : xs) = do
  -- TODO we need to do renaming on the decls
  env' <- makeRecEnv ds
  (L (DoLet ds) loc :) <$> local (const env') (renameDo xs)
renameDo (L (DoBind p v) loc : xs) = do
  L (p', env') patLoc <- keepLoc renamePat p
  let p'' = L p' patLoc
  (\x xs -> L (DoBind p'' x) loc : xs)
    <$> keepLoc renameExpr v
    <*> local (const env') (renameDo xs)
renameDo (L (DoExpr e) loc : xs) =
  (\x xs -> L (DoExpr x) loc : xs) <$> keepLoc renameExpr e <*> renameDo xs

