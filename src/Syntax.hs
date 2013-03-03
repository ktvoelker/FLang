
module Syntax (
  -- * Common types
    No(), Namespace(..), BindName(..), mkBindName, mkUniqueName, namespace, Binder(..)
  , Binding(..), HasBindNames(..)
  -- * Generic expressions and declarations
  , ExprKind(..), ExprTag(..), exprTag, ExprTy, ExprDecl, ExprPrim, Expr(..), mkRef
  , mkMember, mkLet, mkLam, mkRecord, mkApp, mkPrim, mkOpChain, mkToDo, Decl(..)
  -- * Kinds
  , KindPrim(..), KindExpr
  -- * Types
  , TyPrim(..), TyBound(..), mkTyBound, TyCompOp(..), TyDecl(..), mkConstraint
  , mkValField, mkModField, mkTyField, TyExpr, TyBinding
  -- * Modules
  , ModDecl(..), mkBindMod, mkBindSig, mkBindVal, mkBindTy, mkData, mkInfix, ModExpr
  , ModBinding, OpenQual(..), DataMode(..), InfixAssoc(..)
  -- * Values
  , ValPrim(..), mkCase, mkLamCase, mkEInt, mkEFloat, mkEString, mkEChar, mkDo
  , ValDecl(..)
  , mkBindLocalVal, ValExpr, ValBinding, CaseClause(..), mkCaseClause, DoElem(..)
  , mkDoExpr, mkDoBind, mkDoLet
  , Pat(..), mkPatBind, mkPatApp, mkPatIgnore, mkPatParams, mkPatInt, mkPatString
  , mkPatChar
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

type ModBinding = Binding ModK

type ValBinding = Binding ValK

type TyBinding = Binding TyK

type ModExpr = Expr ModK

type ValExpr = Expr ValK

type TyExpr = Expr TyK

type KindExpr = Expr KindK

namespace (BindName _ (x : _)) | isUpper x = NsTys
namespace _ = NsValues

