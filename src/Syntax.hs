
module Syntax (
    module Syntax
  , module Syntax.Pretty
  , module Syntax.Types
  , module Syntax.Util
  ) where

import Syntax.Pretty (SyntaxKind())
import Syntax.Types
import Syntax.Util

type Program = ModExpr

type ModBinding = Binding Mod

type ValBinding = Binding Val

type TyBinding = Binding Ty

type ModDecl = Decl Mod

type ValDecl = Decl Val

type TyDecl = Decl Ty

type ModExpr = Expr Mod

type ValExpr = Expr Val

type TyExpr = Expr Ty

type KindExpr = Expr Kind

