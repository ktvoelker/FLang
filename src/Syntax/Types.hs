
{-# LANGUAGE TemplateHaskell #-}
module Syntax.Types where

import Annotation
import Import

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
  deriving (Eq, Ord, Show)

data DataMode = DataOpen | DataClosed
  deriving (Eq, Ord, Show)

data InfixAssoc = InfixLeft | InfixRight | InfixNone
  deriving (Eq, Ord, Show)

data TyCompOp = OpSubTy | OpSuperTy | OpEqualTy
  deriving (Eq, Ord, Show)

data TyPrim = TyFn | TyAuto | TyEmpty
  deriving (Eq, Ord, Show)

annotateExcept ["Binder"] [d|

  data BindName = BindName String | UniqueName Integer String
    deriving (Eq, Ord, Show)

  data Binder = Binder BindName (Maybe (Expr TyDecl TyPrim))
    deriving (Eq, Ord, Show)

  data TyDecl =
      FieldDecl BindName (Expr TyDecl TyPrim)
    | Constraint (Expr TyDecl TyPrim) TyCompOp (Expr TyDecl TyPrim)
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

  |]

data Binding e = Binding Binder e
  deriving (Eq, Ord, Show)

annotate [d|

  data OpenQual = OpenExcept [BindName] | OpenOnly [BindName]
    deriving (Eq, Ord, Show)

  data ModDecl =
      BindMod (Binding (Expr ModDecl No))
    | BindSig (Binding (Expr SigDecl No))
    | BindVal (Binding (Expr ValDecl ValPrim))
    | BindTy (Binding (Expr TyDecl TyPrim))
    | Data DataMode BindName (Maybe (Expr TyDecl TyPrim)) (Expr TyDecl TyPrim) [ModDecl]
    | Infix InfixAssoc Integer [BindName]
    deriving (Eq, Ord, Show)

  data TyBound = TyBound TyCompOp (Expr TyDecl TyPrim)
    deriving (Eq, Ord, Show)

  data SigDecl =
      SigVal BindName (Expr TyDecl TyPrim)
    | SigTy BindName (Maybe TyBound)
    | SigMod BindName (Expr TyDecl TyPrim)
    deriving (Eq, Ord, Show)

  data ValDecl = BindLocalVal (Binding (Expr ValDecl ValPrim))
    deriving (Eq, Ord, Show)

  data ValPrim =
      LamCase [CaseClause]
    | Case (Expr ValDecl ValPrim) [CaseClause]
    | Do [DoElem]
    | EInt Integer
    | EFloat Rational
    | EString String
    | EChar Char
    deriving (Eq, Ord, Show)

  data CaseClause = CaseClause Pat (Expr ValDecl ValPrim)
    deriving (Eq, Ord, Show)

  data DoElem =
      DoLet [ValDecl]
    | DoBind Pat (Expr ValDecl ValPrim)
    | DoExpr (Expr ValDecl ValPrim)
    deriving (Eq, Ord, Show)

  data Pat =
      PatParams [Pat]
    | PatBind BindName
    | PatApp (Expr ValDecl ValPrim) [Pat]
    | PatInt Integer
    | PatString String
    | PatChar Char
    | PatIgnore
    deriving (Eq, Ord, Show)

  |]
