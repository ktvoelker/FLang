
module Syntax where

newtype BindName = BindName String
  deriving (Eq, Ord, Show)

data OpenQual = OpenExcept [BindName] | OpenOnly [BindName]
  deriving (Eq, Ord, Show)

data DataMode = DataOpen | DataClosed
  deriving (Eq, Ord, Show)

data InfixAssoc = InfixLeft | InfixRight | InfixNone
  deriving (Eq, Ord, Show)

data Binder = Binder BindName (Maybe TyExpr)
  deriving (Eq, Ord, Show)

data ModDecl =
    BindModule Binder ModExpr
  | BindSig Binder SigExpr
  | BindVal Binder ValExpr
  | BindType Binder TyExpr
  | Open ValExpr (Maybe OpenQual)
  | Data DataMode BindName (Maybe TyExpr) TyExpr
  | Type Binder TyExpr
  | Infix InfixAssoc Integer [BindName]
  deriving (Eq, Ord, Show)

data SigDecl = SigDecl
  deriving (Eq, Ord, Show)

data ValDecl = ValDecl
  deriving (Eq, Ord, Show)

data TyDecl = TyDecl
  deriving (Eq, Ord, Show)

data Expr d e =
    Lam [Binder] (Expr d e)
  | App (Expr d e) [Expr d e]
  | Record [d]
  | Ref BindName
  deriving (Eq, Ord, Show)

data ValPrim = ValPrim
  deriving (Eq, Ord, Show)

data TyPrim = TyPrim
  deriving (Eq, Ord, Show)

type ValExpr = Expr ValDecl ValPrim

type TyExpr = Expr TyDecl TyPrim

type ModExpr = Expr ModDecl ()

type SigExpr = Expr SigDecl ()

