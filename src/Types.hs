
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
  } deriving (Eq, Ord, Show)

emptyGlobal root = Global root 0

instance Pretty Global SyntaxKind where
  tokens = tokens . _gRoot

data Env =
  Env
  { _ePath   :: Maybe [BindName]
  , _eLocals :: Map BindName Integer
  } deriving (Eq, Ord, Show)

emptyEnv = Env (Just []) Map.empty

makeLenses [''Global, ''Env]

