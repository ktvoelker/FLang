
{-# LANGUAGE TemplateHaskell #-}
module Syntax.Types where

import Data.Lens.Template

import Annotation
import Import

data Tag = Mod | Val | Ty | Kind
  deriving (Eq, Ord, Enum, Bounded, Show)

type family TyTag (t :: Tag) :: Tag
type instance TyTag Mod = Ty
type instance TyTag Val = Ty
type instance TyTag Ty = Kind

data Lit t where
  KVal    :: Lit Kind
  KMod    :: Lit Kind
  KValFn  :: Lit Kind
  KModFn  :: Lit Kind
  TyFn    :: Lit Ty
  TyAuto  :: Lit Ty
  TyEmpty :: Lit Ty
  LInt    :: Integer -> Lit Val
  LFloat  :: Rational -> Lit Val
  LString :: String -> Lit Val
  LChar   :: Char -> Lit Val

data Namespace = NsTys | NsValues
  deriving (Eq, Ord, Enum, Bounded, Show)

data DataMode = DataOpen | DataClosed
  deriving (Eq, Ord, Enum, Bounded, Show)

data InfixAssoc = InfixLeft | InfixRight | InfixNone
  deriving (Eq, Ord, Enum, Bounded, Show)

data TyCompOp = OpSubTy | OpSuperTy | OpEqualTy
  deriving (Eq, Ord, Enum, Bounded, Show)

data UniqueInfo =
  UniqueInfo
  { _uniqueOrigName  :: String
  , _uniqueGenerated :: Bool
  , _uniqueSection   :: Bool
  } deriving (Show)

makeLenses [''UniqueInfo]

annotateExcept ["Binder", "Binding"] [d|

  data BindName = BindName String | UniqueName Integer UniqueInfo
    deriving (Show)

  data Binder t = Binder BindName (Maybe (Expr (TyTag t)))

  data Expr t where
    Lam       :: [Binder t] -> Expr t -> Expr t
    App       :: Expr t -> [Expr t] -> Expr t
    Record    :: [Decl t] -> Expr t
    Ref       :: BindName -> Expr t
    Member    :: Expr t -> [BindName] -> Expr t
    OpChain   :: Maybe (Expr t) -> [(Expr t, Expr t)] -> Expr t
    Let       :: [Decl t] -> Expr t -> Expr t
    ToDo      :: Expr t
    LamCase   :: [CaseClause] -> Expr Val
    Case      :: Expr Val -> [CaseClause] -> Expr Val
    Do        :: [DoElem] -> Expr Val
    Lit       :: Lit t -> Expr t

  data CaseClause = CaseClause Pat (Expr Val)

  data DoElem =
      DoLet [Decl Val]
    | DoBind Pat (Expr Val)
    | DoExpr (Expr Val)

  data Pat =
      PatParams [Pat]
    | PatBind BindName
    | PatApp (Expr Val) [Pat]
    | PatLit (Lit Val)
    | PatIgnore

  data Decl t where
    Constraint :: Expr Ty -> TyCompOp -> Expr Ty -> Decl Ty
    ValField   :: BindName -> Expr Ty -> Decl Ty
    TyField    :: BindName -> Maybe TyBound -> Decl Ty
    ModField   :: BindName -> Expr Ty -> Decl Ty
    -- TODO: Combine BindLocal and BindVal when the necessary predicate
    -- (TyTag t ~ Ty) is supported by Template Haskell.
    BindLocal  :: Binding Val -> Decl Val
    BindVal    :: Binding Val -> Decl Mod
    BindMod    :: Binding Mod -> Decl Mod
    BindSig    :: Binding Ty -> Decl Mod
    BindTy     :: Binding Ty -> Decl Mod
    Infix      :: InfixAssoc -> Integer -> [BindName] -> Decl Mod
    Data       :: DataMode -> BindName -> Maybe (Expr Ty) -> Expr Ty -> Decl Mod

  data Binding t = Binding (Binder t) (Expr t)

  data TyBound = TyBound TyCompOp (Expr Ty)

  data OpenQual = OpenExcept [BindName] | OpenOnly [BindName]

  |]

srcUniqueName :: Ann -> Integer -> String -> BindName
srcUniqueName ann n xs =
  UniqueName ann n
  $ UniqueInfo
    { _uniqueOrigName  = xs
    , _uniqueGenerated = False
    , _uniqueSection   = False
    }

instance Eq BindName where
  BindName _ xs == BindName _ ys = xs == ys
  UniqueName _ m _ == UniqueName _ n _  = m == n
  _ == _ = False

instance Ord BindName where
  compare BindName{} UniqueName{} = LT
  compare UniqueName{} BindName{} = GT
  compare (BindName _ xs) (BindName _ ys) = compare xs ys
  compare (UniqueName _ m _) (UniqueName _ n _) = compare m n

