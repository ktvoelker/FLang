
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
import Pretty
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

data SyntaxKind = SKText | SKOper | SKSep | SKLBracket | SKRBracket | SKColon
  deriving (Eq, Ord, Show)

instance TokenKind SyntaxKind where
  space _ SKSep = False
  space SKLBracket _ = False
  space _ SKRBracket = False
  space _ SKColon = False
  space _ _ = True

namespace (BindName (x : _)) | isUpper x = NsTys
namespace _ = NsValues

bindingName :: Binding e -> BindName
bindingName (Binding b _) = binderName b

nameIsText :: String -> Bool
nameIsText [] = error "Empty name in nameIsText"
nameIsText (x : _) = x == '_' || isAlpha x

t1 :: (MonadWriter [Token SyntaxKind] m) => SyntaxKind -> String -> m ()
t1 sk xs = tell [Word xs sk]

colon = t1 SKColon ":"

semi = t1 SKSep ";"

tt :: (MonadWriter [Token SyntaxKind] m) => String -> m ()
tt = t1 SKText

tellBrackets :: (MonadWriter [Token SyntaxKind] m) => String -> String -> m () -> m ()
tellBrackets lb rb m = do
  t1 SKLBracket lb
  m
  t1 SKRBracket rb

