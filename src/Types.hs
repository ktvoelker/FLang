
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Lens.Template
import qualified Data.Map as Map
import qualified Data.Set as Set

import Common
import Syntax

data Global =
  Global
  { _gRoot       :: Program
  , _gNextUnique :: Integer
  } deriving (Eq, Ord, Show)

emptyGlobal root = Global root 0

data Env =
  Env
  { _ePath   :: Maybe [BindName]
  , _eLocals :: Map BindName Integer
  } deriving (Eq, Ord, Show)

emptyEnv = Env (Just []) Map.empty

data BR =
  BR
  { _brBinds :: Set Integer
  , _brRefs  :: Set Integer
  } deriving (Eq, Ord, Show)

emptyBR :: BR
emptyBR = BR Set.empty Set.empty

accumBR :: BR -> BR -> BR
accumBR (BR b1 r1) (BR b2 r2) = BR (Set.union b1 b2) (Set.union r1 r2)

makeLenses [''Global, ''Env, ''BR]

