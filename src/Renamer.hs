
module Renamer where

import qualified Data.Map as Map

import Common
import Syntax
import Types

type M = ReaderT Env (StateT Global FM)

-- TODO we have to treat the root like a Record...
rename :: [ModDecl] -> FM Global
rename = execStateT (runReaderT f emptyEnv) . emptyGlobal
  where
    f = gRoot %>>= mapM renameModDecl

allocUnique :: (MonadState Global m) => m Integer
allocUnique = gNextUnique %= (+ 1)

class RenameExpr d e | d -> e where
  renameExpr :: Expr d e -> M (Expr d e)

renameModDecl :: ModDecl -> M ModDecl
renameModDecl (BindMod b) = fmap BindMod . renameBinding $ b
renameModDecl (BindSig b) = fmap BindSig . renameBinding $ b
renameModDecl (BindVal b) = fmap BindVal . renameBinding $ b
renameModDecl (BindTy b) = fmap BindTy . renameBinding $ b
renameModDecl (Data m n p t ds) = do
  p'  <- maybe (return Nothing) (fmap Just . renameExpr) p
  t'  <- renameExpr t
  ds' <- mapM renameModDecl ds
  return $ Data m n p' t' ds'
renameModDecl (Infix a p ns) = fmap (Infix a p) . mapM renameName $ ns

renameBinding :: (RenameExpr d e) => Binding (Expr d e) -> M (Binding (Expr d e))
renameBinding (Binding (Binder n t) e) = do
  t' <- maybe (return Nothing) (fmap Just . renameExpr) t
  fmap (Binding $ Binder n t') . renameExpr $ e

renameName :: BindName -> M BindName
renameName n@(BindName _) = asks _eLocals >>= return . maybe n UniqueName . Map.lookup n
renameName n@(UniqueName _) = return n

-- TODO run renameExpr on the types
withNewNames :: M a -> [Binder] -> M ([Binder], a)
withNewNames inner ns = do
  us  <- mapM (alloc . binderName) ns
  let env = Map.fromList $ zip (map binderName ns) us
  ret <- local (eLocals `modL` Map.union env) inner
  return (zipWith replace ns (map UniqueName us), ret)
  where
    alloc (BindName _)   = allocUnique
    alloc (UniqueName n) = return n
    replace (Binder _ ty) name = Binder name ty

instance RenameExpr ModDecl () where
  renameExpr (Lam bs e) = renameExpr e `withNewNames` bs >>= return . uncurry Lam
  renameExpr _ = undefined

instance RenameExpr SigDecl () where
  renameExpr = undefined

instance RenameExpr ValDecl ValPrim where
  renameExpr = undefined

instance RenameExpr TyDecl TyPrim where
  renameExpr = undefined

