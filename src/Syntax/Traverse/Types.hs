
{-# LANGUAGE TemplateHaskell #-}
module Syntax.Traverse.Types where

import Data.Lens.Template
import qualified Data.Map as Map

import Common
import Syntax.Types

data Context = CtxAny | CtxMod | CtxVal | CtxModTy | CtxValTy | CtxKind
  deriving (Eq, Ord, Show)

tyOfCtx :: Context -> Context
tyOfCtx CtxMod = CtxModTy
tyOfCtx CtxVal = CtxValTy
tyOfCtx CtxModTy = CtxKind
tyOfCtx CtxValTy = CtxKind
tyOfCtx CtxAny = error "tyOfCtx CtxAny"
tyOfCtx CtxKind = error "tyOfCtx CtxKind"

type BindMap e = Map BindName e

data Env e =
  Env
  { _ePath  :: [BindName]
  , _eScope :: BindMap e
  , _eBinds :: BindMap e
  , _eCtx   :: Context
  } deriving (Eq, Ord, Show)

emptyEnv :: Env e
emptyEnv = Env [] Map.empty Map.empty CtxMod

type R e m = ReaderT (Env e) m

data Traversal e m =
  Traversal
  { onDecl       :: forall t. Decl t -> R e m (Decl t)
  , onBinding    :: forall t. Binding t -> R e m (Binding t)
  , onNameRef    :: BindName -> R e m BindName
  , onNameBind   :: BindName -> R e m BindName
  , onBinder     :: forall t. Binder t -> R e m (Binder t)
  , onExpr       :: forall t. Expr t -> R e m (Expr t)
  , onPat        :: Pat -> R e m Pat
  , onCaseClause :: CaseClause -> R e m CaseClause
  , onDoElem     :: DoElem -> R e m DoElem
  , onLamScope   :: BindMap () -> R e m (BindMap e)
  , onRecScope   :: forall t. [Decl t] -> BindMap (Decl t) -> R e m (BindMap e)
  }

emptyTraversal
  :: (Monad m)
  => (BindMap () -> R e m (BindMap e))
  -> (forall t. [Decl t] -> BindMap (Decl t) -> R e m (BindMap e))
  -> Traversal e m
emptyTraversal = Traversal
  return return return
  return return return
  return return return

simpleTraversal :: (Monad m) => Traversal () m
simpleTraversal = emptyTraversal return (const $ return . Map.map (const ()))

makeLenses [''Env]

type M e m = ReaderT (Env e, Traversal e m) m

