
module Infix.Types where

import Common
import Syntax

data Fixity = Fixity InfixAssoc Integer 
  deriving (Show)

data IN t = IO Fixity BindName | IA (Expr t)

