
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
  -- * Locations
  , L(..), LocEnv(..), MonadLoc(..), useLoc, keepLoc, whenJustL
  ) where

import Common
import Pretty

import Text.Parsec (SourcePos())

data L a = L { lVal :: a, lLoc :: SourcePos } deriving (Eq, Ord, Show)

class LocEnv a where
  locLens :: Lens a SourcePos

class (Monad m) => MonadLoc m where
  askLoc   :: m SourcePos
  localLoc :: SourcePos -> m a -> m a

instance (Monad m, LocEnv e) => MonadLoc (ReaderT e m) where
  askLoc = asks (locLens ^$)
  localLoc = local . (locLens ^=)

useLoc :: (MonadLoc m) => (a -> m b) -> L a -> m b
useLoc f (L x loc) = localLoc loc $ f x

keepLoc :: (MonadLoc m) => (a -> m b) -> L a -> m (L b)
keepLoc f l@(L _ loc) = useLoc f l >>= return . flip L loc

whenJustL :: (Monad m) => Maybe (L a) -> (a -> m ()) -> m ()
whenJustL m f = whenJust m $ f . lVal

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

data No

instance Eq No where
  _ == _ = True

instance Ord No where
  compare _ _ = EQ

instance Show No where
  show _ = "()"

instance Pretty No SyntaxKind where
  tokens _ = return ()

data SyntaxKind = SKText | SKOper | SKSep | SKLBracket | SKRBracket | SKColon
  deriving (Eq, Ord, Show)

instance TokenKind SyntaxKind where
  space _ SKSep = False
  space SKLBracket _ = False
  space _ SKRBracket = False
  space _ SKColon = False
  space _ _ = True

instance (Pretty a SyntaxKind) => Pretty (L a) SyntaxKind where
  tokens = tokens . lVal

type Program = ModExpr

data BindName = BindName String | UniqueName Integer String
  deriving (Eq, Ord, Show)

instance HasBindNames BindName where
  bindNames name = [name]

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
  { binderName :: L BindName
  , binderTy   :: Maybe (L TyExpr)
  } deriving (Eq, Ord, Show)

instance HasBindNames Binder where
  bindNames b = [lVal $ binderName b]

instance Pretty Binder SyntaxKind where
  tokens (Binder name Nothing) = tokens name
  tokens (Binder name (Just ty)) = tellBrackets "(" ")" $ do
    tokens name
    colon
    tokens ty

data Binding e = Binding Binder (L e)
  deriving (Eq, Ord, Show)

instance HasBindNames (Binding e) where
  bindNames b = [bindingName b]

instance (Pretty e SyntaxKind) => Pretty (Binding e) SyntaxKind where
  tokens (Binding (Binder name ty) rhs) = do
    tokens name
    whenJust ty $ \ty -> colon >> tokens ty
    tt "is"
    tokens rhs

bindingName :: Binding e -> BindName
bindingName (Binding b _) = lVal $ binderName b

type ModBinding = Binding ModExpr

type SigBinding = Binding SigExpr

type ValBinding = Binding ValExpr

type TyBinding = Binding TyExpr

class HasBindNames a where
  bindNames :: a -> [BindName]

instance (HasBindNames a) => HasBindNames (L a) where
  bindNames = bindNames . lVal

class (HasBindNames a) => Decl a where
  allowInCycles :: a -> Bool

instance (Decl a) => Decl (L a) where
  allowInCycles = allowInCycles . lVal

data ModDecl =
    BindMod ModBinding
  | BindSig SigBinding
  | BindVal ValBinding
  | BindTy TyBinding
  | Data (L DataMode) (L BindName) (Maybe (L TyExpr)) (L TyExpr) [L ModDecl]
  | Infix (L InfixAssoc) (L Integer) [L BindName]
  deriving (Eq, Ord, Show)

instance Pretty BindName SyntaxKind where
  tokens b = case b of
    BindName xs -> f xs
    UniqueName n xs -> f $ xs ++ "_" ++ show n
    where
      f xs =
        if nameIsText xs
        then tell [Word xs SKText]
        else tell [Word ("(" ++ xs ++ ")") SKOper]

instance Pretty ModDecl SyntaxKind where
  tokens (BindMod b) = tt "module" >> tokens b
  tokens (BindSig b) = tt "sig" >> tokens b
  tokens (BindVal b) = tt "val" >> tokens b
  tokens (BindTy b) = tt "type" >> tokens b
  tokens (Data mode name parent ty kids) = do
    tt "data"
    when (lVal mode == DataOpen) $ tt "open"
    tokens name
    whenJust parent $ \parent -> t1 SKOper "<:" >> tokens parent
    when (not $ lVal ty == Prim TyEmpty) $ do
      tt "is"
      tokens ty
    case kids of
      [] -> semi
      _  -> tellBrackets "{" "}" $ mapM_ tokens kids
  tokens (Infix assoc prec bs) = do
    tt "infix"
    case lVal assoc of
      InfixLeft -> tt "left"
      InfixRight -> tt "right"
      InfixNone -> return ()
    tt $ show prec
    mapM_ tokens bs
    semi

instance Decl ModDecl where
  allowInCycles (BindVal _) = True
  allowInCycles (Data _ _ _ _ _) = True
  allowInCycles _ = False

instance HasBindNames ModDecl where
  bindNames (BindMod b) = [bindingName b]
  bindNames (BindSig b) = [bindingName b]
  bindNames (BindVal b) = [bindingName b]
  bindNames (BindTy b) = [bindingName b]
  bindNames (Data _ n _ _ ds) = lVal n : concatMap bindNames ds
  bindNames (Infix _ _ _) = []

data TyBound = TyBound TyCompOp (L TyExpr)
  deriving (Eq, Ord, Show)

data TyCompOp = OpSubTy | OpSuperTy | OpEqualTy
  deriving (Eq, Ord, Show)

data SigDecl =
    SigVal (L BindName) (L TyExpr)
  | SigTy (L BindName) (Maybe (L TyBound))
  | SigMod (L BindName) (L TyExpr)
  deriving (Eq, Ord, Show)

instance Pretty SigDecl SyntaxKind where
  tokens (SigVal name ty) = do
    tt "val"
    tokens name
    colon
    tokens ty
    semi
  tokens (SigTy name bound) = do
    tt "type"
    tokens name
    whenJustL bound $ \(TyBound op ty) -> do
      tokens op
      tokens ty
    semi
  tokens (SigMod name ty) = do
    tt "module"
    tokens name
    colon
    tokens ty
    semi

instance Decl SigDecl where
  allowInCycles = const False

instance HasBindNames SigDecl where
  bindNames (SigVal n _) = [lVal n]
  bindNames (SigTy n _) = [lVal n]
  bindNames (SigMod n _) = [lVal n]

data ValDecl = BindLocalVal ValBinding
  deriving (Eq, Ord, Show)

instance Pretty ValDecl SyntaxKind where
  tokens (BindLocalVal b) = tokens b

instance Decl ValDecl where
  allowInCycles = const True

instance HasBindNames ValDecl where
  bindNames (BindLocalVal b) = [bindingName b]

data TyDecl =
    FieldDecl (L BindName) (L TyExpr)
  | Constraint (L TyExpr) TyCompOp (L TyExpr)
  deriving (Eq, Ord, Show)

instance Pretty TyCompOp SyntaxKind where
  tokens op = t1 SKOper $ case op of
    OpSubTy -> "<:"
    OpSuperTy -> ">:"
    OpEqualTy -> ":"

instance Pretty TyDecl SyntaxKind where
  tokens (FieldDecl name ty) = do
    tokens name
    colon
    tokens ty
    semi
  tokens (Constraint a op b) = do
    tt "with"
    tokens a
    tokens op
    tokens b

instance Decl TyDecl where
  allowInCycles = const False

instance HasBindNames TyDecl where
  bindNames = const []

data Expr d e =
    Lam [L Binder] (L (Expr d e))
  | App (L (Expr d e)) [L (Expr d e)]
  | Record [L d]
  | Ref (L BindName)
  | UniqueRef (L Integer)
  | Member (L (Expr d e)) (L BindName)
  | OpChain (Maybe (L (Expr d e))) [(L (Expr d e), L (Expr d e))]
  | Let [L d] (L (Expr d e))
  | Prim e
  | ToDo
  deriving (Eq, Ord, Show)

instance
  (Pretty d SyntaxKind, Pretty e SyntaxKind)
  => Pretty (Expr d e) SyntaxKind where
  tokens (Lam ps e) = tellBrackets "(" ")" $ do
    tt "fn"
    mapM_ tokens ps
    t1 SKOper "->"
    tokens e
  tokens (App e es) = tokens e >> mapM_ tokens es
  tokens (Record ds) = do
    tt "rec"
    tellBrackets "{" "}" $ mapM_ tokens ds
  tokens (Ref name) = tokens name
  -- TODO it would be useful for these refs to keep the string names
  tokens (UniqueRef n) = tt $ "_?_" ++ show n
  tokens (Member e name) = do
    tokens e
    t1 SKOper "."
    tokens name
  tokens (OpChain x xs) = f $ mapM_ (\(o, e) -> tokens o >> tokens e) xs
    where
      f = case x of
        Nothing -> tellBrackets "(" ")"
        Just x  -> (tokens x >>)
  tokens (Let ds e) = tellBrackets "(" ")" $ do
    tt "let"
    tellBrackets "{" "}" $ mapM_ tokens ds
    tt "in"
    tokens e
  tokens (Prim e) = tokens e
  tokens ToDo = t1 SKOper "?"

data ValPrim =
    LamCase [L CaseClause]
  | Case (L ValExpr) [L CaseClause]
  | Do [L DoElem]
  | EInt (L Integer)
  | EFloat (L Rational)
  | EString (L String)
  | EChar (L Char)
  deriving (Eq, Ord, Show)

instance Pretty ValPrim SyntaxKind where
  tokens (LamCase cs) = do
    tt "fn"
    tellBrackets "{" "}" $ mapM_ tokens cs
  tokens (Case e cs) = do
    tt "case"
    tokens e
    tt "of"
    tellBrackets "{" "}" $ mapM_ tokens cs
  tokens (Do es) = tt "do" >> tellBrackets "{" "}" (mapM_ tokens es)
  tokens (EInt n) = tt $ show n
  tokens (EFloat (L (a :% b) _)) = tellBrackets "(" ")" $ do
    tt $ show a
    t1 SKOper "/"
    tt $ show b
  -- TODO escape properly
  tokens (EString xs) = tt $ "\"" ++ lVal xs ++ "\""
  -- TODO escape properly
  tokens (EChar x) = tt $ "'" ++ [lVal x] ++ "'"

data CaseClause = CaseClause Pat ValExpr
  deriving (Eq, Ord, Show)

instance Pretty CaseClause SyntaxKind where
  tokens (CaseClause pat expr) = do
    tt "if"
    tokens pat
    tt "then"
    tokens expr
    semi

data DoElem =
    DoLet [L ValDecl]
  | DoBind (L Pat) (L ValExpr)
  | DoExpr (L ValExpr)
  deriving (Eq, Ord, Show)

instance Pretty DoElem SyntaxKind where
  tokens (DoLet ds) = do
    tt "let"
    tellBrackets "{" "}" (mapM_ tokens ds)
    semi
  tokens (DoBind pat expr) = do
    tokens pat
    t1 SKOper "<-"
    tokens expr
    semi
  tokens (DoExpr expr) = do
    tokens expr
    semi

data Pat =
    PatParams [L Pat]
  | PatBind (L BindName)
  | PatApp (L ValExpr) [L Pat]
  | PatInt (L Integer)
  | PatString (L String)
  | PatChar (L Char)
  | PatIgnore
  deriving (Eq, Ord, Show)

instance Pretty Pat SyntaxKind where
  tokens (PatParams ps) = mapM_ tokens ps
  tokens (PatBind name) = tokens name
  tokens (PatApp expr ps) = tellBrackets "(" ")" $ do
    tokens expr
    mapM_ tokens ps
  tokens (PatInt n) = tokens $ EInt n
  tokens (PatString xs) = tokens $ EString xs
  tokens (PatChar x) = tokens $ EChar x
  tokens PatIgnore = tt "_"

instance HasBindNames Pat where
  bindNames (PatParams ps) = ps >>= bindNames
  bindNames (PatBind n) = [lVal n]
  bindNames (PatApp _ ps) = ps >>= bindNames
  bindNames _ = []

data TyPrim = TyFn | TyAuto | TyEmpty
  deriving (Eq, Ord, Show)

instance Pretty TyPrim SyntaxKind where
  -- TODO figure out how to get the arrows to appear infix
  tokens TyFn = tt "(->)"
  tokens TyAuto = t1 SKOper "*"
  tokens TyEmpty = tt "(#?EMPTY?#)"

type ModExpr = Expr ModDecl No

type SigExpr = Expr SigDecl No

type ValExpr = Expr ValDecl ValPrim

type TyExpr = Expr TyDecl TyPrim

