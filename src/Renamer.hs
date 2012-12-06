
module Renamer where

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

renameBinding :: Binding (Expr d e) -> M (Binding (Expr d e))
renameBinding (Binding (Binder n t) e) = do
  t' <- maybe (return Nothing) (fmap Just . renameExpr) t
  fmap (Binding $ Binder n t') . renameExpr $ e

renameName :: BindName -> M BindName
renameName = undefined

renameExpr :: Expr d e -> M (Expr d e)
renameExpr = undefined

