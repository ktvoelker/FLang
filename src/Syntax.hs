
module Syntax (
    module Syntax
  , module Syntax.Decl
  , module Syntax.HasBindNames
  , module Syntax.Pretty
  , module Syntax.Types
  ) where

import Common
import Syntax.Decl
import Syntax.HasBindNames
import Syntax.Pretty (SyntaxKind())
import Syntax.Types

type Program = ModExpr

type ModBinding = Binding Mod

type ValBinding = Binding Val

type TyBinding = Binding Ty

type ModExpr = Expr Mod

type ValExpr = Expr Val

type TyExpr = Expr Ty

type KindExpr = Expr Kind

namespace (BindName _ (x : _)) | isUpper x = NsTys
namespace _ = NsValues

