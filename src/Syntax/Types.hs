
{-# LANGUAGE TemplateHaskell #-}
module Syntax.Types where

import Annotation
import Import

data ExprKind = ModK | ValK | TyK | KindK | NoK

data ExprTag (k :: ExprKind) = ExprTag

type family ExprTy (k :: ExprKind) :: ExprKind

type family ExprDecl (k :: ExprKind) :: *

type family ExprPrim (k :: ExprKind) :: *

data No

instance Eq No where
  _ == _ = True

instance Ord No where
  compare _ _ = EQ

instance Show No where
  show _ = "()"

data KindPrim = KVal | KMod | KValFn | KModFn
  deriving (Eq, Ord, Enum, Bounded, Show)

data Namespace = NsTys | NsValues
  deriving (Eq, Ord, Enum, Bounded, Show)

data DataMode = DataOpen | DataClosed
  deriving (Eq, Ord, Enum, Bounded, Show)

data InfixAssoc = InfixLeft | InfixRight | InfixNone
  deriving (Eq, Ord, Enum, Bounded, Show)

data TyCompOp = OpSubTy | OpSuperTy | OpEqualTy
  deriving (Eq, Ord, Enum, Bounded, Show)

data TyPrim = TyFn | TyAuto | TyEmpty
  deriving (Eq, Ord, Enum, Bounded, Show)

annotateExcept ["Binder"] [d|

  data BindName = BindName String | UniqueName Integer String
    deriving (Eq, Ord, Show)

  data Binder (k :: ExprKind) = Binder BindName (Maybe (Expr k))

  data TyBound = TyBound TyCompOp (Expr TyK)

  data TyDecl =
      Constraint (Expr TyK) TyCompOp (Expr TyK)
    | ValField BindName (Expr TyK)
    | TyField BindName (Maybe TyBound)
    | ModField BindName (Expr TyK)

  data Expr (k :: ExprKind) =
      Lam [Binder k] (Expr k)
    | App (Expr k) [Expr k]
    | Record [ExprDecl k]
    | Ref BindName
    | UniqueRef Integer
    | Member (Expr k) BindName
    | OpChain (Maybe (Expr k)) [(Expr k, Expr k)]
    | Let [ExprDecl k] (Expr k)
    | Prim (ExprPrim k)
    | ToDo

  exprTag :: Expr k -> ExprTag k
  exprTag = const ExprTag

  |]

data Binding (k :: ExprKind) = Binding (Binder k) (Expr k)

annotate [d|

  data OpenQual = OpenExcept [BindName] | OpenOnly [BindName]

  data ModDecl =
      BindMod (Binding ModK)
    | BindSig (Binding TyK)
    | BindVal (Binding ValK)
    | BindTy (Binding TyK)
    | Data DataMode BindName (Maybe (Expr TyK)) (Expr TyK) [ModDecl]
    | Infix InfixAssoc Integer [BindName]

  data ValDecl = BindLocalVal (Binding ValK)

  data ValPrim =
      LamCase [CaseClause]
    | Case (Expr ValK) [CaseClause]
    | Do [DoElem]
    | EInt Integer
    | EFloat Rational
    | EString String
    | EChar Char

  data CaseClause = CaseClause Pat (Expr ValK)

  data DoElem =
      DoLet [ValDecl]
    | DoBind Pat (Expr ValK)
    | DoExpr (Expr ValK)

  data Pat =
      PatParams [Pat]
    | PatBind BindName
    | PatApp (Expr ValK) [Pat]
    | PatInt Integer
    | PatString String
    | PatChar Char
    | PatIgnore

  type instance ExprTy ModK  = TyK
  type instance ExprTy ValK  = TyK
  type instance ExprTy TyK   = KindK
  type instance ExprTy KindK = NoK

  type instance ExprDecl ModK  = ModDecl
  type instance ExprDecl ValK  = ValDecl
  type instance ExprDecl TyK   = TyDecl
  type instance ExprDecl KindK = No

  type instance ExprPrim ModK  = No
  type instance ExprPrim ValK  = ValPrim
  type instance ExprPrim TyK   = TyPrim
  type instance ExprPrim KindK = KindPrim

  |]
