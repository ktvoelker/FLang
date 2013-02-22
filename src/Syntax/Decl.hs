
module Syntax.Decl where

import Common
import Syntax.HasBindNames
import Syntax.Types

class (HasBindNames a) => Decl a where
  allowInCycles :: a -> Bool

instance Decl ModDecl where
  allowInCycles (BindVal _) = True
  allowInCycles (Data _ _ _ _ _) = True
  allowInCycles _ = False

instance Decl SigDecl where
  allowInCycles = const False

instance Decl ValDecl where
  allowInCycles = const True

instance Decl TyDecl where
  allowInCycles = const False

