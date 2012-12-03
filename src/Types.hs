
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Lens.Template
import qualified Data.Map as Map

import Common
import Syntax

data Global =
  Global
  { _gRoot :: [ModDecl]
  , _gMods :: Map ModName [ModDecl]
  , _gSigs :: Map ModName [SigDecl]
  } deriving (Eq, Ord, Show)

emptyGlobal root = Global root Map.empty Map.empty

data Env =
  Env
  { _eMods :: Map BindName ModExpr
  , _eSigs :: Map BindName SigExpr
  , _eTys  :: Map BindName TyExpr
  , _eVals :: Map BindName ValExpr
  } deriving (Eq, Ord, Show)

emptyEnv = Env Map.empty Map.empty Map.empty Map.empty

makeLenses [''Global, ''Env]

