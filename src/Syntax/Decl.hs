
module Syntax.Decl where

import Common
import Syntax.Types

allowInCycles :: Decl t -> Bool
allowInCycles (BindLocal _ _) = True
allowInCycles (BindVal _ _) = True
allowInCycles (Data _ _ _ _ _ _) = True
allowInCycles _ = False

