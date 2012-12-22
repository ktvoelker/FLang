
module Syntax where

import Common
import Pretty

data No

instance Eq No where
  _ == _ = True

instance Ord No where
  compare _ _ = EQ

instance Show No where
  show _ = "()"

instance Pretty No SyntaxKind where
  tokens _ = []

data SyntaxKind = SKText | SKOper | SKSep | SKLBracket | SKRBracket
  deriving (Eq, Ord, Show)

instance TokenKind SyntaxKind where
  space _ SKSep = False
  space SKLBracket _ = False
  space _ SKRBracket = False
  space _ _ = True

type Program = ModExpr

data BindName = BindName String | UniqueName Integer String
  deriving (Eq, Ord, Show)

data Namespace = NsTys | NsValues
  deriving (Eq, Ord, Show)

namespace (BindName (x : _)) | isUpper x = NsTys
namespace _ = NsValues

data OpenQual = OpenExcept [BindName] | OpenOnly [BindName]
  deriving (Eq, Ord, Show)

data DataMode = DataOpen | DataClosed
  deriving (Eq, Ord, Show)

data InfixAssoc = InfixLeft | InfixRight | InfixNone
  deriving (Eq, Ord, Show)

data Binder =
  Binder
  { binderName :: BindName
  , binderTy   :: Maybe TyExpr
  } deriving (Eq, Ord, Show)

data Binding e = Binding Binder e
  deriving (Eq, Ord, Show)

bindingName :: Binding e -> BindName
bindingName (Binding b _) = binderName b

type ModBinding = Binding ModExpr

type SigBinding = Binding SigExpr

type ValBinding = Binding ValExpr

type TyBinding = Binding TyExpr

class Decl a where
  allowInCycles :: a -> Bool
  declBindNames :: a -> [BindName]

data ModDecl =
    BindMod ModBinding
  | BindSig SigBinding
  | BindVal ValBinding
  | BindTy TyBinding
  | Data DataMode BindName (Maybe TyExpr) TyExpr [ModDecl]
  | Infix InfixAssoc Integer [BindName]
  deriving (Eq, Ord, Show)

instance Pretty ModDecl SyntaxKind where
  tokens = undefined

instance Decl ModDecl where
  allowInCycles (BindVal _) = True
  allowInCycles (Data _ _ _ _ _) = True
  allowInCycles _ = False
  declBindNames (BindMod b) = [bindingName b]
  declBindNames (BindSig b) = [bindingName b]
  declBindNames (BindVal b) = [bindingName b]
  declBindNames (BindTy b) = [bindingName b]
  declBindNames (Data _ n _ _ ds) = n : concatMap declBindNames ds
  declBindNames (Infix _ _ _) = []

data TyBound = TyBound TyCompOp TyExpr
  deriving (Eq, Ord, Show)

data TyCompOp = OpSubTy | OpSuperTy | OpEqualTy
  deriving (Eq, Ord, Show)

data SigDecl =
    SigVal BindName TyExpr
  | SigTy BindName (Maybe TyBound)
  | SigMod BindName TyExpr
  deriving (Eq, Ord, Show)

instance Decl SigDecl where
  allowInCycles = const False
  declBindNames (SigVal n _) = [n]
  declBindNames (SigTy n _) = [n]
  declBindNames (SigMod n _) = [n]

data ValDecl = BindLocalVal ValBinding
  deriving (Eq, Ord, Show)

instance Decl ValDecl where
  allowInCycles = const True
  declBindNames (BindLocalVal b) = [bindingName b]

data TyDecl =
    FieldDecl BindName TyExpr
  | Constraint TyExpr TyCompOp TyExpr
  deriving (Eq, Ord, Show)

instance Decl TyDecl where
  allowInCycles = const False
  declBindNames = const []

data Expr d e =
    Lam [Binder] (Expr d e)
  | App (Expr d e) [Expr d e]
  | Record [d]
  | Ref BindName
  | UniqueRef Integer
  | Member (Expr d e) BindName
  | OpChain (Maybe (Expr d e)) [(Expr d e, Expr d e)]
  | Let [d] (Expr d e)
  | Prim e
  | ToDo
  deriving (Eq, Ord, Show)

instance
  (Pretty d SyntaxKind, Pretty e SyntaxKind)
  => Pretty (Expr d e) SyntaxKind where
  tokens = undefined

data ValPrim =
    LamCase [CaseClause]
  | Case ValExpr [CaseClause]
  | Do [DoElem]
  | EInt Integer
  | EFloat Rational
  | EString String
  | EChar Char
  deriving (Eq, Ord, Show)

data CaseClause = CaseClause Pat ValExpr
  deriving (Eq, Ord, Show)

data DoElem =
    DoLet [ValDecl]
  | DoBind Pat ValExpr
  | DoExpr ValExpr
  deriving (Eq, Ord, Show)

data Pat =
    PatParams [Pat]
  | PatBind BindName
  | PatApp ValExpr [Pat]
  | PatInt Integer
  | PatString String
  | PatChar Char
  | PatIgnore
  deriving (Eq, Ord, Show)

data TyPrim = TyFn | TyAuto | TyEmpty
  deriving (Eq, Ord, Show)

type ModExpr = Expr ModDecl No

type SigExpr = Expr SigDecl No

type ValExpr = Expr ValDecl ValPrim

type TyExpr = Expr TyDecl TyPrim

