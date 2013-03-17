
{-# LANGUAGE TemplateHaskell #-}
module Infix.Types where

import Data.Lens.Template

import Common
import Syntax

data Fixity = Fixity InfixAssoc Integer 
  deriving (Show)

data IN =
  IN
  { _inFixity :: Fixity
  , _inOp     :: BindName
  } deriving (Show)

makeLenses [''IN]

