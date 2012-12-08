
module Syntax where

import Common

type Program = ModExpr

data ModName =
    UserModName [BindName]
  | GenModName Integer String
  deriving (Eq, Ord, Show)

data BindName = BindName String | UniqueName Integer
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

type ModBinding = Binding ModExpr

type SigBinding = Binding SigExpr

type ValBinding = Binding ValExpr

type TyBinding = Binding TyExpr

data ModDecl =
    BindMod ModBinding
  | BindSig SigBinding
  | BindVal ValBinding
  | BindTy TyBinding
  | Data DataMode BindName (Maybe TyExpr) TyExpr [ModDecl]
  | Infix InfixAssoc Integer [BindName]
  deriving (Eq, Ord, Show)

data TyBound = TyBound TyCompOp TyExpr
  deriving (Eq, Ord, Show)

data TyCompOp = OpSubTy | OpSuperTy | OpEqualTy
  deriving (Eq, Ord, Show)

data SigDecl =
    SigVal BindName TyExpr
  | SigTy BindName (Maybe TyBound)
  | SigMod BindName TyExpr
  deriving (Eq, Ord, Show)

data ValDecl = BindLocalVal ValBinding
  deriving (Eq, Ord, Show)

data TyDecl =
    FieldDecl BindName TyExpr
  | Constraint TyExpr TyCompOp TyExpr
  deriving (Eq, Ord, Show)

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

data ValPrim =
    LamCase [FnClause]
  | Case ValExpr [CaseClause]
  | Do [DoElem]
  | EInt Integer
  | EFloat Rational
  | EString String
  | EChar Char
  deriving (Eq, Ord, Show)

data FnClause = FnClause [Pat] ValExpr
  deriving (Eq, Ord, Show)

data CaseClause = CaseClause Pat ValExpr
  deriving (Eq, Ord, Show)

data DoElem =
    DoLet [ValDecl]
  | DoBind Pat ValExpr
  | DoExpr ValExpr
  deriving (Eq, Ord, Show)

data Pat =
    PatBind BindName
  | PatApp ValExpr [Pat]
  | PatInt Integer
  | PatString String
  | PatChar Char
  | PatIgnore
  deriving (Eq, Ord, Show)

data TyPrim = TyFn | TyAuto | TyEmpty
  deriving (Eq, Ord, Show)

type ModExpr = Expr ModDecl ()

type SigExpr = Expr SigDecl ()

type ValExpr = Expr ValDecl ValPrim

type TyExpr = Expr TyDecl TyPrim

