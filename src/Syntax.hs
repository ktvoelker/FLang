
module Syntax (
  -- * Common types
    No(), Namespace(..), BindName(..), namespace, Binder(..), Binding(..)
  , HasBindNames(..)
  -- * Generic expressions and declarations
  , Expr(..), Decl(..)
  -- * Sigs
  , SigDecl(..), SigExpr, SigBinding
  -- * Modules
  , ModDecl(..), ModExpr, ModBinding, OpenQual(..), DataMode(..), InfixAssoc(..)
  -- * Types
  , TyPrim(..), TyBound(..), TyCompOp(..), TyDecl(..), TyExpr, TyBinding
  -- * Values
  , ValPrim(..), ValDecl(..), ValExpr, ValBinding, CaseClause(..), DoElem(..), Pat(..)
  -- * Type aliases
  , Program
  -- * Pretty-printing
  , SyntaxKind()
  ) where

import Common
import Syntax.Decl
import Syntax.HasBindNames
import Syntax.Pretty
import Syntax.Types

type Program = ModExpr

type ModBinding = Binding ModExpr

type SigBinding = Binding SigExpr

type ValBinding = Binding ValExpr

type TyBinding = Binding TyExpr

type ModExpr = Expr ModDecl No

type SigExpr = Expr SigDecl No

type ValExpr = Expr ValDecl ValPrim

type TyExpr = Expr TyDecl TyPrim

namespace (BindName _ (x : _)) | isUpper x = NsTys
namespace _ = NsValues

