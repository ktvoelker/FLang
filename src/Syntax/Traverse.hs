
module Syntax.Traverse where

import qualified Data.Map as Map

import Common
import Syntax.Traverse.Types
import Syntax

type C m = (Functor m, Applicative m, Monad m)

withBindsInScope :: (C m) => R e m b -> R e m b
-- TODO rewrite using lenses
withBindsInScope =
  local $ \env -> env { _eScope = Map.union (_eBinds env) (_eScope env) }

layer :: (C m) => Lens (Traversal e m) (b -> R e m b) -> M e m b -> M e m b
layer lens monad = do
  editor <- asks $ lens . snd
  withReaderT fst $ fmap editor monad

mapDecl :: (C m) => Decl t -> M e m (Decl t)
mapDecl = layer onDecl . \case
  Constraint ann a o b ->
    Constraint ann <$> mapExpr a <*> pure o <*> mapExpr b
  ValField a n e -> ValField a <$> mapNameBind n <*> mapExpr e
  TyField a n ty -> TyField a <$> mapNameBind n <*> ty'
    where
      ty' = case ty of
        Nothing -> pure Nothing
        Just (TyBound a o e) -> Just . TyBound a o <$> mapExpr e
  ModField a n e -> ModField a <$> mapNameBind n <*> mapExpr e
  BindLocal a b -> BindLocal a <$> mapBinding b
  BindMod a b -> BindMod a <$> mapBinding b
  BindSig a b -> BindSig a <$> mapBinding b
  BindVal a b -> BindVal a <$> mapBinding b
  BindTy a b -> BindTy a <$> mapBinding b
  Data a m n p t ds -> do
    n'  <- mapNameBind n
    p'  <- maybe (return Nothing) (fmap Just . mapExpr) p
    t'  <- mapExpr t
    ds' <- mapM mapDecl ds
    return $ Data a m n' p' t' ds'
  Infix ann a p ns -> Infix ann a p <$> mapM mapNameRef ns

mapBinding :: (C m) => Binding k -> M e m (Binding k)
mapBinding = layer onBinding . \case
  Binding (Binder n t) e -> do
    n' <- mapNameBind n
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

makeRecEnv :: (C m) => [Decl t] -> M e m (Env e)
makeRecEnv ds = do
  env <- ask
  newPairs <-
    layer onRecScope
    $ return
    $ Map.fromList
    $ ds >>= \d -> zip (binds d) (repeat d)
  let f = (^%= Map.union newPairs)
  return $ f eBinds $ f eScope env

makeBindEnv :: (C m) => (Binds a) => [a] -> M e m (Env e)
makeBindEnv ds = do
  env <- ask
  newPairs <-
    layer onLamScope
    $ return
    $ Map.fromList
    $ ds >>= \d -> zip (binds d) (repeat ())
  let f = (^%= Map.union newPairs)
  return $ f eBinds env

mapExpr :: (C m) => Expr t -> M e m (Expr t)
mapExpr = layer onExpr . \case
  Lam a bs e -> do
    env' <- makeBindEnv bs
    local (const env') $ Lam a <$> mapBinders bs <*> withBindsInScope (mapExpr e)
  App a f as -> App a <$> mapExpr f <*> mapM mapExpr as
  Record a ds -> do
    -- TODO is this wrong?
    -- Should the local environment be applied to the layering function?
    env' <- makeRecEnv ds
    local (const env') $ Record a <$> mapM mapDecl ds
  Ref a n -> Ref a <$> mapNameRef n
  Member a e n -> Member a <$> mapExpr e <*> pure n
  OpChain a e os -> OpChain a <$> mapM mapExpr e <*> mapM f os
    where
      f (a, b) = (,) <$> mapExpr a <*> mapExpr b
  Let a ds e -> do
    env' <- makeRecEnv ds
    local (const env') $ Let a <$> mapM mapDecl ds <*> mapExpr e
  e@(ToDo _) -> return e
  LamCase a xs -> LamCase a <$> mapM mapCaseClause xs
  Case a e xs -> Case a <$> mapExpr e <*> mapM mapCaseClause xs
  Do a xs -> Do a <$> mapDo xs
  lit@(Lit _ _) -> return lit

mapCaseClause :: (C m) => CaseClause -> M e m CaseClause
mapCaseClause = \case
  CaseClause a p v -> do
    (p', env') <- mapPat p
    local (const env') $ withBindsInScope $ CaseClause a p' <$> mapExpr v

mapPat :: (C m) => Pat -> M e m (Pat, Env e)
mapPat = \case
  pat -> do
    env' <- makeBindEnv [pat]
    local (const env') $ (, env') <$> mapPat' pat

mapPat' :: (C m) => Pat -> M e m Pat
mapPat' = \case
  PatParams a ps -> PatParams a <$> mapM mapPat' ps
  PatApp a e ps -> PatApp a <$> mapExpr e <*> mapM mapPat' ps
  PatBind a b -> PatBind a <$> mapNameBind b
  lit -> return lit

mapDo :: (C m) => [DoElem] -> M e m [DoElem]
mapDo = todo
{-
mapDo [] = return []
mapDo (DoLet a ds : xs) = do
  -- TODO we need to do renaming on the decls
  env' <- makeRecEnv ds
  (DoLet a ds :) <$> local (const env') (mapDo xs)
mapDo (DoBind a p v : xs) = do
  (p', env') <- mapPat p
  (:) <$> (DoBind a p' <$> mapExpr v) <*> local (const env') (mapDo xs)
mapDo (DoExpr a e : xs) = (:) <$> (DoExpr a <$> mapExpr e) <*> mapDo xs
-}

