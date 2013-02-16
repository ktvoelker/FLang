
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Lens.Template
import qualified Data.Map as Map
import Text.Parsec.Pos (initialPos, SourcePos())

import Common
import Pretty
import Syntax

data Global =
  Global
  { _gRoot       :: Program
  , _gNextUnique :: Integer
  } deriving (Eq, Ord, Show)

emptyGlobal root = Global root 0

instance Pretty Global SyntaxKind where
  tokens = tokens . _gRoot

data Env =
  Env
  { _ePath  :: Maybe [BindName]
  , _eScope :: Map BindName Integer
  , _eBinds :: Map BindName Integer
  , _eLoc   :: SourcePos
  } deriving (Eq, Ord, Show)

emptyEnv :: Env
emptyEnv = Env (Just []) Map.empty Map.empty (initialPos "")

withBindsInScope :: (MonadReader Env m) => m a -> m a
withBindsInScope =
  local $ \env -> env { _eScope = Map.union (_eBinds env) (_eScope env) }

makeLenses [''Global, ''Env]

