
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Lens.Template
import qualified Data.Map as Map

import Common
import Pretty
import Syntax

data Global =
  Global
  { _gRoot       :: Program
  , _gNextUnique :: Integer
  }

emptyGlobal root = Global root 0

instance Pretty Global SyntaxKind where
  tokens = tokens . _gRoot

data Env =
  Env
  { _ePath  :: Maybe [BindName]
  , _eScope :: Map BindName Integer
  , _eBinds :: Map BindName Integer
  } deriving (Eq, Ord, Show)

emptyEnv :: Env
emptyEnv = Env (Just []) Map.empty Map.empty

withBindsInScope :: (MonadReader Env m) => m a -> m a
withBindsInScope =
  local $ \env -> env { _eScope = Map.union (_eBinds env) (_eScope env) }

makeLenses [''Global, ''Env]

