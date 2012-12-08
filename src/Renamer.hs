
module Renamer where

import qualified Data.Map as Map

import Common
import Syntax
import Types

type M = ReaderT Env (StateT Global FM)

type MRec = StateT (Env, Global) FM

rename :: Program -> FM Global
rename = execStateT (runReaderT f emptyEnv) . emptyGlobal
  where
    f = gRoot %>>= renameExpr

allocUnique :: (MonadState Global m) => m Integer
allocUnique = gNextUnique %= (+ 1)

class RenameExpr d e | d -> e where
  renameExpr :: Expr d e -> M (Expr d e)

-- TODO split this into two functions
-- 1. renameModDeclLHS
-- 2. renameModDeclRHS

renameModDeclLHS :: ModDecl -> MRec ModDecl
renameModDeclLHS (BindMod b) = fmap BindMod . renameBindingLHS $ b
renameModDeclLHS (BindSig b) = fmap BindSig . renameBindingLHS $ b
renameModDeclLHS (BindVal b) = fmap BindVal . renameBindingLHS $ b
renameModDeclLHS (BindTy b) = fmap BindTy . renameBindingLHS $ b
renameModDeclLHS (Data m n p t ds) = do
  ds' <- mapM renameModDeclLHS ds
  n'  <- focus sndLens allocUnique
  _   <- eLocals . fstLens %= Map.insert n n'
  return $ Data m (UniqueName n') p t ds'
renameModDeclLHS i@(Infix _ _ _) = return i

renameBindingLHS :: (RenameExpr d e) => Binding (Expr d e) -> MRec (Binding (Expr d e))
renameBindingLHS (Binding (Binder n t) e) = do
  n' <- focus sndLens allocUnique
  _  <- eLocals . fstLens %= Map.insert n n'
  return $ Binding (Binder (UniqueName n') t) e

renameModDeclRHS :: ModDecl -> M ModDecl
renameModDeclRHS = undefined

{-
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
-}

renameBindingRHS :: (RenameExpr d e) => Binding (Expr d e) -> M (Binding (Expr d e))
renameBindingRHS (Binding (Binder n t) e) = do
  n' <- renameName n
  t' <- maybe (return Nothing) (fmap Just . renameExpr) t
  fmap (Binding $ Binder n' t') . renameExpr $ e

renameName :: BindName -> M BindName
renameName n@(BindName _) = asks _eLocals >>= return . maybe n UniqueName . Map.lookup n
renameName n@(UniqueName _) = return n

withNewNames :: M a -> [Binder] -> M ([Binder], a)
withNewNames inner ns = do
  us  <- mapM (alloc . binderName) ns
  let env = Map.fromList $ zip (map binderName ns) us
  ret <- local (eLocals `modL` Map.union env) inner
  ns' <- zipWithM replace ns (map UniqueName us)
  return (ns', ret)
  where
    alloc (BindName _)   = allocUnique
    alloc (UniqueName n) = return n
    replace (Binder _ ty) name =
      fmap (Binder name)
      . maybe (return Nothing) (fmap Just . renameExpr)
      $ ty

instance RenameExpr ModDecl () where
  renameExpr (Lam bs e) = uncurry Lam <$> (renameExpr e `withNewNames` bs)
  renameExpr (App f as) = App <$> renameExpr f <*> mapM renameExpr as
  renameExpr (Record ds) = do
    env   <- ask
    state <- get
    (ds', (env', state')) <-
      lift
      . lift
      . runStateT (mapM renameModDeclLHS ds)
      $ (env, state)
    put state'
    Record <$> local (const env') (mapM renameModDeclRHS ds')
  renameExpr _ = undefined

instance RenameExpr SigDecl () where
  renameExpr = undefined

instance RenameExpr ValDecl ValPrim where
  renameExpr = undefined

instance RenameExpr TyDecl TyPrim where
  renameExpr = undefined

