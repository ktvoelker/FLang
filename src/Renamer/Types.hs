
{-# LANGUAGE TemplateHaskell #-}
module Renamer.Types where

import Data.Lens.Template
import qualified Data.Map as Map

import Common
import Syntax
import Syntax.Traverse.Types

data RenamerState =
  RenamerState
  { _rsNextUnique :: Integer
  , _rsRefs       :: BindMap (Set Integer)
  , _rsProgram    :: Program
  }

emptyRenamerState :: Program -> RenamerState
emptyRenamerState = RenamerState 0 Map.empty

makeLenses [''RenamerState]

