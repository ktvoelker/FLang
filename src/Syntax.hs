
module Syntax (
  -- * Common types
    No(), Namespace(..), BindName(..), mkBindName, mkUniqueName, namespace, Binder(..)
  , Binding(..), HasBindNames(..)
  -- * Generic expressions and declarations
  , Expr(..), mkRef, mkMember, mkLet, mkLam, mkRecord, mkApp, mkPrim, mkOpChain, mkToDo
  , Decl(..)
  -- * Sigs
  , SigDecl(..), mkSigMod, mkSigVal, mkSigTy, SigExpr, SigBinding
  -- * Modules
  , ModDecl(..), mkBindMod, mkBindSig, mkBindVal, mkBindTy, mkData, mkInfix, ModExpr
  , ModBinding, OpenQual(..), DataMode(..), InfixAssoc(..)
  -- * Types
  , TyPrim(..), TyBound(..), mkTyBound, TyCompOp(..), TyDecl(..), mkFieldDecl
  , mkConstraint, TyExpr, TyBinding
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

