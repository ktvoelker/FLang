
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Lens.Template
import qualified Data.Map as Map

import Common
import Syntax

data Global =
  Global
  { _gRoot :: [ModDecl]
  , _gSigs :: Map ModName SigBinding
  } deriving (Eq, Ord, Show)

emptyGlobal root = Global root Map.empty

data Env =
  Env
  { _ePath :: Maybe [BindName]
  , _eMods :: Map BindName ModExpr
  , _eTys  :: Map BindName TyExpr
  , _eVals :: Map BindName ValExpr
  } deriving (Eq, Ord, Show)

emptyEnv = Env (Just []) Map.empty Map.empty Map.empty

makeLenses [''Global, ''Env]

