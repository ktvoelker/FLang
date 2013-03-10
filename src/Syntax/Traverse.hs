
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

pushBind :: (MonadReader (Env e, t) m) => BindName -> m a -> m a
pushBind name = local $ (ePath . fstLens) ^%= (name :)

mapDecl :: (C m) => Decl t -> M e m (Decl t)
mapDecl = layer onDecl . \case
  Constraint ann a o b ->
    Constraint ann <$> mapExpr a <*> pure o <*> mapExpr b
  ValField a n e -> do
    n' <- mapNameBind n
    pushBind n' $ ValField a n' <$> mapExpr e
  TyField a n ty -> do
    n' <- mapNameBind n
    pushBind n' $ TyField a n' <$> ty'
    where
      ty' = case ty of
        Nothing -> pure Nothing
        Just (TyBound a o e) -> Just . TyBound a o <$> mapExpr e
  ModField a n e -> do
    n' <- mapNameBind n
    pushBind n' $ ModField a n' <$> mapExpr e
  BindLocal a b -> BindLocal a <$> mapBinding b
  BindMod a b -> BindMod a <$> mapBinding b
  BindSig a b -> BindSig a <$> mapBinding b
  BindVal a b -> BindVal a <$> mapBinding b
  BindTy a b -> BindTy a <$> mapBinding b
  Data a m n p t ds -> do
    n' <- mapNameBind n
    pushBind n' $ do
      p'  <- maybe (return Nothing) (fmap Just . mapExpr) p
      t'  <- mapExpr t
      ds' <- mapM mapDecl ds
      return $ Data a m n' p' t' ds'
  Infix ann a p ns -> Infix ann a p <$> mapM mapNameRef ns

mapBinding :: (C m) => Binding k -> M e m (Binding k)
mapBinding = layer onBinding . \case
  Binding (Binder n t) e -> do
    n' <- mapNameBind n
    pushBind n' $ do
      t' <- maybe (return Nothing) (fmap Just . mapExpr) t
      Binding (Binder n' t') <$> mapExpr e

mapNameRef :: (C m) => BindName -> M e m BindName
mapNameRef = layer onNameRef . return

mapNameBind :: (C m) => BindName -> M e m BindName
mapNameBind = layer onNameBind . return

mapBinders :: (C m) => [Binder k] -> M e m [Binder k]
mapBinders = mapM mapBinder

mapBinder :: (C m) => Binder k -> M e m (Binder k)
mapBinder = layer onBinder . \case
  Binder name ty -> Binder <$> mapNameBind name <*> mapM mapExpr ty

makeRecEnv :: forall e m t. (C m) => [Decl t] -> M e m (Env e)
makeRecEnv ds = do
  env <- asks fst
  let bMap = Map.fromList $ ds >>= \d -> zip (binds d) (repeat d)
  newPairs <- layer onRecScope $ return bMap
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
    Member a e n -> Member a <$> mapExpr e <*> pure n
    OpChain a e os -> OpChain a <$> mapM mapExpr e <*> mapM f os
      where
        f (a, b) = (,) <$> mapExpr a <*> mapExpr b
    e@ToDo{} -> return e
    LamCase a xs -> LamCase a <$> mapM mapCaseClause xs
    Case a e xs -> Case a <$> mapExpr e <*> mapM mapCaseClause xs
    Do a xs -> Do a <$> mapDo xs
    lit@Lit{} -> return lit
    Lam{} -> impossible
    Record{} -> impossible
    Let{} -> impossible

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

