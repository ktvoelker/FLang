
module Renamer where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Common
import Syntax
import Types

import Renamer.Sorter

type M = ReaderT Env (StateT Global (AccumT BR FM))

rename :: Program -> FM Global
rename p =
  evalAccumT (execStateT (runReaderT f emptyEnv) $ emptyGlobal $ p) emptyBR accumBR
  where
    f = do
      result <- gRoot %>>= renameExpr
      lift2 getAccum >>= lift3 . report . EInternal . show
      return result

type MRec = StateT (Env, Global) FM

makeEnv :: MRec a -> M (a, Env)
makeEnv m = do
  env   <- ask
  state <- get
  (x, (env', state')) <- lift3 . runStateT m $ (env, state)
  put state'
  return (x, env')

allocUnique :: (MonadState Global m) => m Integer
allocUnique = gNextUnique %= (+ 1)

insertBind :: (Monad m) => Integer -> AccumT BR m ()
insertBind n = getAccum >>= putAccum . (brBinds ^%= Set.insert n)

insertBindName :: BindName -> M ()
insertBindName (UniqueName z _) = lift2 . insertBind $ z
insertBindName (BindName xs) =
  lift3 . report . EInternal $ "Unexpected BindName: " ++ xs

insertRef :: (Monad m) => Integer -> AccumT BR m ()
insertRef n = getAccum >>= putAccum . (brRefs ^%= Set.insert n)

getBinds :: (Monad m) => AccumT BR m (Set Integer)
getBinds = (brBinds ^$) <$> getAccum

getRefs :: (Monad m) => AccumT BR m (Set Integer)
getRefs = (brRefs ^$) <$> getAccum

class (Show a) => RenameDecl a where
  allowCycles :: a -> Bool
  renameLHS   :: a -> MRec a
  renameRHS   :: a -> M a

class RenamePrim a where
  renamePrim :: a -> M a

instance RenameDecl ModDecl where
  allowCycles _ = False
  -- LHS
  renameLHS (BindMod b) = BindMod <$> renameBindingLHS b
  renameLHS (BindSig b) = BindSig <$> renameBindingLHS b
  renameLHS (BindVal b) = BindVal <$> renameBindingLHS b
  renameLHS (BindTy b) = BindTy <$> renameBindingLHS b
  renameLHS (Data m n p t ds) = do
    ds' <- mapM renameLHS ds
    n'  <- renameNameLHS n
    return $ Data m n' p t ds'
  renameLHS i@(Infix _ _ _) = return i
  -- RHS
  renameRHS (BindMod b) = BindMod <$> renameBindingRHS b
  renameRHS (BindSig b) = BindSig <$> renameBindingRHS b
  renameRHS (BindVal b) = BindVal <$> renameBindingRHS b
  renameRHS (BindTy b) = BindTy <$> renameBindingRHS b
  renameRHS (Data m n p t ds) = do
    insertBindName n
    p'  <- maybe (return Nothing) (fmap Just . renameExpr) p
    t'  <- renameExpr t
    ds' <- mapM renameRHS ds
    return $ Data m n p' t' ds'
  renameRHS (Infix a p ns) = (Infix a p) <$> mapM renameName ns

instance RenameDecl SigDecl where
  allowCycles _ = False
  renameLHS (SigVal n e) = SigVal <$> renameNameLHS n <*> pure e
  renameLHS (SigTy n t) = SigTy <$> renameNameLHS n <*> pure t
  renameLHS (SigMod n e) = SigMod <$> renameNameLHS n <*> pure e
  renameRHS (SigVal n e) = do
    insertBindName n
    SigVal n <$> renameExpr e
  renameRHS t@(SigTy n Nothing) = insertBindName n >> return t
  renameRHS (SigTy n (Just (TyBound o e))) = do
    insertBindName n
    SigTy n . Just . TyBound o <$> renameExpr e
  renameRHS (SigMod n e) = do
    insertBindName n
    SigMod n <$> renameExpr e

instance RenameDecl ValDecl where
  allowCycles _ = True
  renameLHS (BindLocalVal b) = BindLocalVal <$> renameBindingLHS b
  renameRHS (BindLocalVal b) = BindLocalVal <$> renameBindingRHS b

instance RenameDecl TyDecl where
  allowCycles _ = False
  renameLHS = return
  renameRHS (FieldDecl n e) = FieldDecl n <$> renameExpr e
  renameRHS (Constraint a o b) = Constraint <$> renameExpr a <*> pure o <*> renameExpr b

renameNameLHS :: BindName -> MRec BindName
renameNameLHS n@(UniqueName _ _) = return n
renameNameLHS n@(BindName xs) = do
  n' <- focus sndLens allocUnique
  _  <- eLocals . fstLens %= Map.insert n n'
  return $ UniqueName n' xs

renameBindingLHS
  :: (RenameDecl d, RenamePrim e) => Binding (Expr d e) -> MRec (Binding (Expr d e))
renameBindingLHS (Binding (Binder n t) e) = do
  n' <- renameNameLHS n
  return $ Binding (Binder n' t) e

renameBindingRHS
  :: (RenameDecl d, RenamePrim e) => Binding (Expr d e) -> M (Binding (Expr d e))
renameBindingRHS (Binding (Binder n t) e) = do
  insertBindName n
  t' <- maybe (return Nothing) (fmap Just . renameExpr) t
  Binding (Binder n t') <$> renameExpr e

renameName :: BindName -> M BindName
renameName n@(BindName xs) = do
  z <- Map.lookup n <$> asks _eLocals
  case z of
    Nothing -> do
      lift3 . report . EUnbound $ xs
      return n
    Just z -> do
      lift2 . insertRef $ z
      return $ UniqueName z xs
renameName n@(UniqueName z _) = do
  lift2 . insertRef $ z
  return n

renameBinders :: [Binder] -> M ([Binder], Env)
renameBinders = mapM renameBinderTy >=> makeEnv . mapM renameBinderName

renameBinderTy :: Binder -> M Binder
renameBinderTy (Binder name ty) = Binder name <$> mapM renameExpr ty

renameBinderName :: Binder -> MRec Binder
renameBinderName (Binder name ty) = Binder <$> renameNameLHS name <*> pure ty

makeRecEnv :: (RenameDecl d) => [d] -> M ([d], Env)
makeRecEnv = makeEnv . mapM renameLHS

renameSortDecls :: (RenameDecl d) => [d] -> M [d]
renameSortDecls [] = return []
renameSortDecls ds@(d : _) =
  mapM (branch . renameRHS) ds
  >>= lift3 . sortDecls (allowCycles $ undefined `asTypeOf` d)

renameExpr :: (RenameDecl d, RenamePrim e) => Expr d e -> M (Expr d e)
renameExpr (Lam bs e) = do
  (bs', env') <- renameBinders bs
  mapM_ insertBindName . map binderName $ bs'
  Lam bs' <$> local (const env') (renameExpr e)
renameExpr (App f as) = App <$> renameExpr f <*> mapM renameExpr as
renameExpr (Record ds) = do
  (ds', env') <- makeRecEnv ds
  Record <$> local (const env') (renameSortDecls ds')
renameExpr (Ref n) = Ref <$> renameName n
renameExpr e@(UniqueRef _) = return e
renameExpr (Member e n) = Member <$> renameExpr e <*> pure n
renameExpr (OpChain e os) = OpChain <$> mapM renameExpr e <*> mapM f os
  where
    f (a, b) = (,) <$> renameExpr a <*> renameExpr b
renameExpr (Let ds e) = do
  (ds', env') <- makeRecEnv ds
  let loc = local (const env')
  ds'' <- loc $ renameSortDecls ds'
  e' <- loc $ renameExpr e
  Let <$> pure ds'' <*> pure e'
renameExpr (Prim p) = Prim <$> renamePrim p
renameExpr ToDo = return ToDo

instance RenamePrim () where
  renamePrim () = do
    lift3 . report $ EInternal "Unexpected Prim found in ModExpr or SigExpr"
    return ()

instance RenamePrim ValPrim where
  renamePrim (LamCase xs) = LamCase <$> mapM renameCaseClause xs
  renamePrim (Case e xs) = Case <$> renameExpr e <*> mapM renameCaseClause xs
  renamePrim (Do xs) = Do <$> renameDo xs
  renamePrim lit = return lit

instance RenamePrim TyPrim where
  renamePrim = return

renameCaseClause (CaseClause p v) = do
  (p', env') <- renamePat p
  CaseClause p' <$> local (const env') (renameExpr v)

renamePat :: Pat -> M (Pat, Env)
renamePat p = do
  result@(p', _) <- renamePatExprs >=> makeEnv . renamePatBinds $ p
  mapM_ insertBindName . getPatBinds $ p'
  return result

renamePatExprs (PatParams ps) = PatParams <$> mapM renamePatExprs ps
renamePatExprs (PatApp e ps) = PatApp <$> renameExpr e <*> mapM renamePatExprs ps
renamePatExprs p = return p

renamePatBinds (PatBind n) = PatBind <$> renameNameLHS n
renamePatBinds (PatParams ps) = PatParams <$> mapM renamePatBinds ps
renamePatBinds (PatApp e ps) = PatApp e <$> mapM renamePatBinds ps
renamePatBinds lit = return lit

getPatBinds (PatBind n) = [n]
getPatBinds (PatParams ps) = ps >>= getPatBinds
getPatBinds (PatApp _ ps) = ps >>= getPatBinds
getPatBinds _ = []

renameDo [] = return []
renameDo (DoLet ds : xs) = do
  (ds', env') <- makeRecEnv ds
  (DoLet ds' :) <$> local (const env') (renameDo xs)
renameDo (DoBind p v : xs) = do
  (p', env') <- renamePat p
  (:) <$> (DoBind p' <$> renameExpr v) <*> local (const env') (renameDo xs)
renameDo (DoExpr e : xs) = (:) <$> (DoExpr <$> renameExpr e) <*> renameDo xs

