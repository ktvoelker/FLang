
module Infix.Types where

import Common
import Pretty
import Syntax

data FixityRank = SystemLow | User | SystemHigh
  deriving (Eq, Ord, Enum, Bounded, Show)

data Fixity = Fixity InfixAssoc FixityRank Integer
  deriving (Show)

data IN t = IO Fixity (Expr t) | IA (Expr t)

instance Show (IN t) where
  showsPrec p (IO f e) =
      ("IO (" ++)
    . showsPrec p f
    . (") (" ++)
    . (pretty e ++)
    . (")" ++)
  showsPrec _ (IA e) =
      ("IA (" ++)
    . (pretty e ++)
    . (")" ++)

