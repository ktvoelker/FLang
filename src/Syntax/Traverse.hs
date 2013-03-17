
module Syntax.Traverse
  ( BindMap, Env(..), ePath, eBinds, eScope, Traversal(..), emptyTraversal, mapProgram
  ) where

import qualified Data.Map as Map

import Common
import Syntax.Traverse.Types
import Syntax

mapProgram
  :: (Functor m, Applicative m, Monad m)
  => Traversal e m
  -> Expr t
  -> m (Expr t)
mapProgram t = flip runReaderT (emptyEnv, t) . mapExpr

type C m = (Functor m, Applicative m, Monad m)

withBindsInScope :: (C m) => M e m b -> M e m b
withBindsInScope =
  local $ (fstLens ^%=) $ \env -> (eScope ^%= Map.union (eBinds ^$ env)) env

layer :: (C m) => (Traversal e m -> a -> R e m b) -> M e m a -> M e m b
layer getEditor monad = do
  editors <- asks snd
  let editorR = getEditor editors
  let editorM = withReaderT fst . editorR
  monad >>= editorM

layer2 :: (C m) => (Traversal e m -> a -> b -> R e m c) -> a -> M e m b -> M e m c
layer2 getEditor arg monad = do
  editors <- asks snd
  let editorR = getEditor editors
  let editorM = withReaderT fst . editorR arg
  monad >>= editorM

pushBind :: (MonadReader (Env e, t) m) => BindName -> m a -> m a
pushBind name = local $ (ePath . fstLens) ^%= (name :)

inCtx :: (MonadReader (Env e, t) m) => Context -> m a -> m a
inCtx = local . (eCtx . fstLens ^=)

inTyOfCtx :: (MonadReader (Env e, t) m) => m a -> m a
inTyOfCtx = local $ eCtx . fstLens ^%= tyOfCtx

mapDecl :: (C m) => Decl t -> M e m (Decl t)
mapDecl = layer onDecl . \case
  -- TODO context
  Constraint ann a o b ->
    Constraint ann <$> mapExpr a <*> pure o <*> mapExpr b
  ValField a n e -> inCtx CtxVal $ do
    n' <- mapNameBind n
    pushBind n' $ ValField a n' <$> (inTyOfCtx $ mapExpr e)
  TyField a n ty -> inCtx CtxValTy $ do
    n' <- mapNameBind n
    pushBind n' $ TyField a n' <$> ty'
    where
      ty' = case ty of
        Nothing -> pure Nothing
        Just (TyBound a o e) -> Just . TyBound a o <$> mapExpr e
  ModField a n e -> inCtx CtxMod $ do
    n' <- mapNameBind n
    pushBind n' $ ModField a n' <$> (inTyOfCtx $ mapExpr e)
  BindLocal a b -> inCtx CtxVal $ BindLocal a <$> mapBinding b
  BindMod a b -> inCtx CtxMod $ BindMod a <$> mapBinding b
  BindSig a b -> inCtx CtxModTy $ BindSig a <$> mapBinding b
  BindVal a b -> inCtx CtxVal $ BindVal a <$> mapBinding b
  BindTy a b -> inCtx CtxValTy $ BindTy a <$> mapBinding b
  Data a m n p t ds -> inCtx CtxValTy $ do
    n' <- mapNameBind n
    pushBind n' $ do
      p'  <- maybe (return Nothing) (fmap Just . mapExpr) p
      t'  <- mapExpr t
      ds' <- mapM mapDecl ds
      return $ Data a m n' p' t' ds'
  Infix ann a p ns -> Infix ann a p <$> mapM mapNameRef ns

mapBinding :: (C m) => Binding k -> M e m (Binding k)
mapBinding (Binding b e) = layer onBinding $ Binding <$> mapBinder b <*> mapExpr e

mapNameRef :: (C m) => BindName -> M e m BindName
mapNameRef = layer onNameRef . return

mapNameBind :: (C m) => BindName -> M e m BindName
mapNameBind = layer onNameBind . return

mapBinders :: (C m) => [Binder k] -> M e m [Binder k]
mapBinders = mapM mapBinder

mapBinder :: (C m) => Binder k -> M e m (Binder k)
mapBinder (Binder name ty) =
  layer onBinder $ Binder <$> mapNameBind name <*> (inTyOfCtx $ mapM mapExpr ty)

makeRecEnv :: forall e m t. (C m) => [Decl t] -> M e m (Env e)
makeRecEnv ds = do
  env <- asks fst
  let bMap = Map.fromList $ ds >>= \d -> zip (binds d) (repeat d)
  newPairs <- layer2 onRecScope ds $ return bMap
  let f = (^%= Map.union newPairs)
  return $ f eBinds $ f eScope env

makeBindEnv :: (C m) => (Binds a) => [a] -> M e m (Env e)
makeBindEnv ds = do
  env <- asks fst
  let bMap = Map.fromList $ ds >>= \d -> zip (binds d) (repeat ())
  newPairs <- layer onLamScope $ return bMap
  let f = (^%= Map.union newPairs)
  return $ f eBinds env

mapExpr :: (C m) => Expr t -> M e m (Expr t)
mapExpr = \case
  Lam a bs e -> do
    env' <- makeBindEnv bs
    layer onExpr
      $ local (fstLens ^= env')
      $ Lam a <$> mapBinders bs <*> withBindsInScope (mapExpr e)
  Record a ds -> do
    env' <- makeRecEnv ds
    layer onExpr $ local (fstLens ^= env') $ Record a <$> mapM mapDecl ds
  Let a ds e -> do
    env' <- makeRecEnv ds
    layer onExpr $ local (fstLens ^= env') $ Let a <$> mapM mapDecl ds <*> mapExpr e
  e -> layer onExpr $ case e of
    App a f as -> App a <$> mapExpr f <*> mapM mapExpr as
    Ref a n -> Ref a <$> mapNameRef n
    Member a e n -> inCtx CtxAny $ Member a <$> mapExpr e <*> pure n
    OpChain a e os -> OpChain a <$> mapM mapExpr e <*> mapM f os
      where
        f (a, b) = (,) <$> mapExpr a <*> mapExpr b
    e@ToDo{} -> return e
    LamCase a xs -> LamCase a <$> mapM mapCaseClause xs
    Case a e xs -> Case a <$> mapExpr e <*> mapM mapCaseClause xs
    Do a xs -> Do a <$> mapDo xs
    lit@Lit{} -> return lit
    Lam{} -> impossible "reached unreachable pattern Lam{} in mapExpr"
    Record{} -> impossible "reached unreachable pattern Record{} in mapExpr"
    Let{} -> impossible "reached unreachable pattern Let{} in mapExpr"

mapCaseClause :: (C m) => CaseClause -> M e m CaseClause
mapCaseClause = \case
  CaseClause a p v -> do
    (p', env') <- mapPat p
    local (fstLens ^= env') $ withBindsInScope $ CaseClause a p' <$> mapExpr v

mapPat :: (C m) => Pat -> M e m (Pat, Env e)
mapPat = \case
  pat -> do
    env' <- makeBindEnv [pat]
    local (fstLens ^= env') $ (, env') <$> mapPat' pat

mapPat' :: (C m) => Pat -> M e m Pat
mapPat' = \case
  PatParams a ps -> PatParams a <$> mapM mapPat' ps
  PatApp a e ps -> PatApp a <$> mapExpr e <*> mapM mapPat' ps
  PatBind a b -> PatBind a <$> mapNameBind b
  lit -> return lit

-- TODO make it tail-recursive
mapDo :: (C m) => [DoElem] -> M e m [DoElem]
mapDo [] = return []
mapDo (DoLet a ds : xs) = do
  env' <- makeRecEnv ds
  local (fstLens ^= env')
    $ (:) <$> (layer onDoElem $ DoLet a <$> mapM mapDecl ds) <*> mapDo xs
mapDo (DoBind a p v : xs) = do
  (p', env') <- mapPat p
  local (fstLens ^= env')
    $ (:) <$> (layer onDoElem $ DoBind a p' <$> mapExpr v) <*> mapDo xs
mapDo (DoExpr a e : xs) =
  (:) <$> (layer onDoElem $ DoExpr a <$> mapExpr e) <*> mapDo xs

