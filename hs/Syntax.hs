
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

data TyBound = TyBound TyCompOp TyExpr
  deriving (Eq, Ord, Show)

data TyCompOp = OpSubTy | OpSuperTy | OpEqualTy
  deriving (Eq, Ord, Show)

data SigDecl =
    SigVal BindName TyExpr
  | SigType BindName (Maybe TyBound)
  | SigModule BindName TyExpr
  deriving (Eq, Ord, Show)

data ValDecl = BindLocalVal Binder ValExpr
  deriving (Eq, Ord, Show)

data TyDecl = TyDecl
  deriving (Eq, Ord, Show)

data Expr d e =
    Lam [Binder] (Expr d e)
  | App (Expr d e) [Expr d e]
  | Record [d]
  | Ref BindName
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

data DoElem = DoElem
  deriving (Eq, Ord, Show)

data Pat = Pat
  deriving (Eq, Ord, Show)

data TyPrim = TyPrim
  deriving (Eq, Ord, Show)

type ValExpr = Expr ValDecl ValPrim

type TyExpr = Expr TyDecl TyPrim

type ModExpr = Expr ModDecl ()

type SigExpr = Expr SigDecl ()

