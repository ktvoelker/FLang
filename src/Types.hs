
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Lens.Template

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

makeLenses [''Global]

