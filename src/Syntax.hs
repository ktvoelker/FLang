
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
  tokens _ = return ()

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

instance (Pretty e SyntaxKind) => Pretty (Binding e) SyntaxKind where
  tokens (Binding (Binder name ty) rhs) = do
    tokens name
    whenJust ty $ \ty -> t1 SKOper ":" >> tokens ty
    tt "is"
    tokens rhs

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

nameIsText :: String -> Bool
nameIsText [] = error "Empty name in nameIsText"
nameIsText (x : _) = x == '_' || isAlpha x

instance Pretty BindName SyntaxKind where
  tokens b = case b of
    BindName xs -> f xs
    UniqueName n xs -> f $ xs ++ "_" ++ show n
    where
      f xs = tell [Word xs $ if nameIsText xs then SKText else SKOper]

t1 :: (MonadWriter [Token SyntaxKind] m) => SyntaxKind -> String -> m ()
t1 sk xs = tell [Word xs sk]

tt :: (MonadWriter [Token SyntaxKind] m) => String -> m ()
tt = t1 SKText

tellBrackets :: (MonadWriter [Token SyntaxKind] m) => String -> String -> m () -> m ()
tellBrackets lb rb m = do
  t1 SKLBracket lb
  m
  t1 SKRBracket rb

instance Pretty ModDecl SyntaxKind where
  tokens (BindMod b) = tt "module" >> tokens b
  tokens (BindSig b) = tt "sig" >> tokens b
  tokens (BindVal b) = tt "val" >> tokens b
  tokens (BindTy b) = tt "type" >> tokens b
  tokens (Data mode name parent ty kids) = do
    tt "data"
    when (mode == DataOpen) $ tt "open"
    tokens name
    whenJust parent $ \parent -> t1 SKOper "<:" >> tokens parent
    when (not $ ty == Prim TyEmpty) $ do
      tt "is"
      tokens ty
    case kids of
      [] -> t1 SKSep ";"
      _  -> tellBrackets "{" "}" $ mapM_ tokens kids
  tokens (Infix assoc prec bs) = do
    tt "infix"
    case assoc of
      InfixLeft -> tt "left"
      InfixRight -> tt "right"
      InfixNone -> return ()
    tt $ show prec
    mapM_ tokens bs
    t1 SKSep ";"

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

instance Pretty SigDecl SyntaxKind where
  tokens = undefined

instance Decl SigDecl where
  allowInCycles = const False
  declBindNames (SigVal n _) = [n]
  declBindNames (SigTy n _) = [n]
  declBindNames (SigMod n _) = [n]

data ValDecl = BindLocalVal ValBinding
  deriving (Eq, Ord, Show)

instance Pretty ValDecl SyntaxKind where
  tokens (BindLocalVal b) = tokens b

instance Decl ValDecl where
  allowInCycles = const True
  declBindNames (BindLocalVal b) = [bindingName b]

data TyDecl =
    FieldDecl BindName TyExpr
  | Constraint TyExpr TyCompOp TyExpr
  deriving (Eq, Ord, Show)

instance Pretty TyDecl SyntaxKind where
  tokens (FieldDecl name ty) = do
    tokens name
    t1 SKOper ":"
    tokens ty
    t1 SKSep ";"
  tokens (Constraint a op b) = do
    tt "with"
    tokens a
    t1 SKOper $ case op of
      OpSubTy -> "<:"
      OpSuperTy -> ">:"
      OpEqualTy -> ":"
    tokens b

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

instance Pretty ValPrim SyntaxKind where
  tokens (LamCase _) = undefined
  tokens (Case _ _) = undefined
  tokens (Do es) = tt "do" >> tellBrackets "{" "}" (mapM_ tokens es)
  tokens (EInt n) = tt $ show n
  tokens (EFloat (a :% b)) = tellBrackets "(" ")" $ do
    tt $ show a
    t1 SKOper "/"
    tt $ show b
  tokens (EString _) = undefined
  tokens (EChar _) = undefined

data CaseClause = CaseClause Pat ValExpr
  deriving (Eq, Ord, Show)

data DoElem =
    DoLet [ValDecl]
  | DoBind Pat ValExpr
  | DoExpr ValExpr
  deriving (Eq, Ord, Show)

instance Pretty DoElem SyntaxKind where
  tokens = undefined

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

instance Pretty TyPrim SyntaxKind where
  tokens = undefined

type ModExpr = Expr ModDecl No

type SigExpr = Expr SigDecl No

type ValExpr = Expr ValDecl ValPrim

type TyExpr = Expr TyDecl TyPrim

