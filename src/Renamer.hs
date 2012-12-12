
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

class RenameDecl a where
  renameLHS :: a -> MRec a
  renameRHS :: a -> M a

class RenamePrim a where
  renamePrim :: a -> M a

instance RenameDecl ModDecl where
  -- LHS
  renameLHS (BindMod b) = fmap BindMod . renameBindingLHS $ b
  renameLHS (BindSig b) = fmap BindSig . renameBindingLHS $ b
  renameLHS (BindVal b) = fmap BindVal . renameBindingLHS $ b
  renameLHS (BindTy b) = fmap BindTy . renameBindingLHS $ b
  renameLHS (Data m n p t ds) = do
    ds' <- mapM renameLHS ds
    n'  <- focus sndLens allocUnique
    _   <- eLocals . fstLens %= Map.insert n n'
    return $ Data m (UniqueName n') p t ds'
  renameLHS i@(Infix _ _ _) = return i
  -- RHS
  renameRHS (BindMod b) = fmap BindMod . renameBindingRHS $ b
  renameRHS (BindSig b) = fmap BindSig . renameBindingRHS $ b
  renameRHS (BindVal b) = fmap BindVal . renameBindingRHS $ b
  renameRHS (BindTy b) = fmap BindTy . renameBindingRHS $ b
  renameRHS (Data m n p t ds) = do
    p'  <- maybe (return Nothing) (fmap Just . renameExpr) p
    t'  <- renameExpr t
    ds' <- mapM renameRHS ds
    return $ Data m n p' t' ds'
  renameRHS (Infix a p ns) = fmap (Infix a p) . mapM renameName $ ns

instance RenameDecl SigDecl where
  renameLHS (SigVal n e) = SigVal <$> renameNameLHS n <*> pure e
  renameLHS (SigTy n t) = SigTy <$> renameNameLHS n <*> pure t
  renameLHS (SigMod n e) = SigMod <$> renameNameLHS n <*> pure e
  renameRHS (SigVal n e) = SigVal n <$> renameExpr e
  renameRHS t@(SigTy _ Nothing) = return t
  renameRHS (SigTy n (Just (TyBound o e))) = SigTy n . Just . TyBound o <$> renameExpr e
  renameRHS (SigMod n e) = SigMod n <$> renameExpr e

instance RenameDecl ValDecl where
  renameLHS (BindLocalVal b) = BindLocalVal <$> renameBindingLHS b
  renameRHS (BindLocalVal b) = BindLocalVal <$> renameBindingRHS b

instance RenameDecl TyDecl where
  renameLHS = return
  renameRHS (FieldDecl n e) = FieldDecl n <$> renameExpr e
  renameRHS (Constraint a o b) = Constraint <$> renameExpr a <*> pure o <*> renameExpr b

renameNameLHS :: BindName -> MRec BindName
renameNameLHS n = do
  n' <- focus sndLens allocUnique
  _  <- eLocals . fstLens %= Map.insert n n'
  return $ UniqueName n'

renameBindingLHS
  :: (RenameDecl d, RenamePrim e) => Binding (Expr d e) -> MRec (Binding (Expr d e))
renameBindingLHS (Binding (Binder n t) e) = do
  n' <- renameNameLHS n
  return $ Binding (Binder n' t) e

renameBindingRHS
  :: (RenameDecl d, RenamePrim e) => Binding (Expr d e) -> M (Binding (Expr d e))
renameBindingRHS (Binding (Binder n t) e) = do
  t' <- maybe (return Nothing) (fmap Just . renameExpr) t
  fmap (Binding $ Binder n t') . renameExpr $ e

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

makeRecEnv :: (RenameDecl d) => [d] -> M ([d], Env)
makeRecEnv ds = do
  env   <- ask
  state <- get
  (ds', (env', state')) <- lift2 . runStateT (mapM renameLHS ds) $ (env, state)
  put state'
  return (ds', env')

renameExpr :: (RenameDecl d, RenamePrim e) => Expr d e -> M (Expr d e)
renameExpr (Lam bs e) = uncurry Lam <$> (renameExpr e `withNewNames` bs)
renameExpr (App f as) = App <$> renameExpr f <*> mapM renameExpr as
renameExpr (Record ds) = do
  (ds', env') <- makeRecEnv ds
  Record <$> local (const env') (mapM renameRHS ds')
renameExpr (Ref n) = Ref <$> renameName n
renameExpr e@(UniqueRef _) = return e
renameExpr (Member e n) = Member <$> renameExpr e <*> pure n
renameExpr (OpChain e os) = OpChain <$> mapM renameExpr e <*> mapM f os
  where
    f (a, b) = (,) <$> renameExpr a <*> renameExpr b
renameExpr (Let ds e) = do
  (ds', env') <- makeRecEnv ds
  (ds'', e')  <-
    local (const env')
    $ (,) <$> mapM renameRHS ds' <*> renameExpr e
  return $ Let ds'' e'
renameExpr (Prim p) = Prim <$> renamePrim p
renameExpr ToDo = return ToDo

instance RenamePrim () where
  renamePrim () = do
    lift2 . report $ EInternal "Unexpected Prim found in ModExpr or SigExpr"
    return ()

instance RenamePrim ValPrim where
  renamePrim (LamCase xs) = LamCase <$> mapM renameFnClause xs
  renamePrim (Case e xs) = Case <$> renameExpr e <*> mapM renameCaseClause xs
  renamePrim (Do xs) = Do <$> renameDo xs
  renamePrim lit = return lit

instance RenamePrim TyPrim where
  renamePrim = return

-- WRONG
-- We have to allocate new names for pattern bindings and include them in a local
-- environment on the RHS of each pattern. Same for FnClause.
renameCaseClause (CaseClause p v) = CaseClause <$> renamePat p <*> renameExpr v

-- WRONG
renameFnClause (FnClause ps v) = FnClause <$> mapM renamePat ps <*> renameExpr v

-- WRONG
-- Names in patterns are always bindings, not uses.
-- Use MRec for renamePat so that we can accumulate all the bindings in a pattern
-- statefully.
renamePat (PatBind n) = PatBind <$> renameName n
renamePat (PatApp e ps) = PatApp <$> renameExpr e <*> mapM renamePat ps
renamePat lit = return lit

renameDo [] = return []
-- TODO
-- Once renamePat has been fixed, this will be easier.
renameDo (DoLet ds : xs) = undefined
renameDo (DoBind p v : xs) = undefined
renameDo (DoExpr e : xs) = (:) <$> (DoExpr <$> renameExpr e) <*> renameDo xs

