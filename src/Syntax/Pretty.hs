
{-# LANGUAGE UndecidableInstances #-}
module Syntax.Pretty where

import Common
import Pretty
import Syntax.Types

data SyntaxKind = SKText | SKOper | SKSep | SKLBracket | SKRBracket | SKColon
  deriving (Eq, Ord, Show)

instance TokenKind SyntaxKind where
  space _ SKSep = False
  space SKLBracket _ = False
  space _ SKRBracket = False
  space _ SKColon = False
  space _ _ = True

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

instance (Pretty (Expr (TyTag k)) SyntaxKind, Pretty (Expr k) SyntaxKind)
  => Pretty (Binder k) SyntaxKind where
  tokens (Binder name Nothing) = tokens name
  tokens (Binder name (Just ty)) = tellBrackets "(" ")" $ do
    tokens name
    colon
    tokens ty

instance (Pretty (Expr (TyTag k)) SyntaxKind, Pretty (Expr k) SyntaxKind)
  => Pretty (Binding k) SyntaxKind where
  tokens (Binding (Binder name ty) rhs) = do
    tokens name
    whenJust ty $ \ty -> colon >> tokens ty
    tt "is"
    tokens rhs

instance Pretty BindName SyntaxKind where
  tokens b = case b of
    BindName _ xs -> f xs
    UniqueName _ n info -> f $ (uniqueOrigName ^$ info) ++ "_" ++ show n
    where
      f xs =
        if nameIsText xs
        then tell [Word xs SKText]
        else tell [Word ("(" ++ xs ++ ")") SKOper]

instance Pretty (Decl t) SyntaxKind where
  tokens (ValField _ name ty) = do
    tt "val" -- TODO no "val" when inside a val record
    tokens name
    colon
    tokens ty
    semi
  tokens (TyField _ name bound) = do
    tt "type"
    tokens name
    whenJust bound $ \(TyBound _ op ty) -> do
      tokens op
      tokens ty
    semi
  tokens (ModField _ name ty) = do
    tt "module"
    tokens name
    colon
    tokens ty
    semi
  tokens (Constraint _ a op b) = do
    tt "with"
    tokens a
    tokens op
    tokens b
  tokens (BindLocal _ b) = tokens b
  tokens (BindMod _ b) = tt "module" >> tokens b
  tokens (BindSig _ b) = tt "sig" >> tokens b
  tokens (BindVal _ b) = tt "val" >> tokens b
  tokens (BindTy _ b) = tt "type" >> tokens b
  tokens (Infix _ assoc prec bs) = do
    tt "infix"
    case assoc of
      InfixLeft -> tt "left"
      InfixRight -> tt "right"
      InfixNone -> return ()
    tt $ show prec
    mapM_ tokens bs
    semi
  tokens (Data _ mode name parent ty) = do
    tt "data"
    when (mode == DataOpen) $ tt "open"
    tokens name
    whenJust parent $ \parent -> t1 SKOper "<:" >> tokens parent
    case ty of
      Lit _ TyEmpty -> return ()
      _ -> tt "is" >> tokens ty
    semi

instance Pretty TyCompOp SyntaxKind where
  tokens op = t1 SKOper $ case op of
    OpSubTy -> "<:"
    OpSuperTy -> ">:"
    OpEqualTy -> ":"

instance Pretty (Expr t) SyntaxKind where
  tokens (Lam _ ps e) = tellBrackets "(" ")" $ do
    tt "fn"
    mapM_ tokens ps
    t1 SKOper "->"
    tokens e
  tokens (App _ e es) = tokens e >> mapM_ tokens es
  tokens (Record _ ds) = do
    tt "rec"
    tellBrackets "{" "}" $ mapM_ tokens ds
  tokens (Ref _ name) = tokens name
  tokens (Member _ e names) = do
    tokens e
    mapM_ (\name -> t1 SKOper "." >> tokens name) names
  tokens (OpChain _ x xs o) = f $ mapM_ (\(o, e) -> tokens o >> tokens e) xs
    where
      f = case (x, o) of
        (Nothing, Nothing) -> tellBrackets "(" ")"
        (Nothing, Just o) -> tellBrackets "(" ")" . (>> tokens o)
        (Just x, Nothing) -> (tokens x >>)
        (Just x, Just o) -> tellBrackets "(" ")" . (tokens x >>) . (>> tokens o)
  tokens (Let _ ds e) = tellBrackets "(" ")" $ do
    tt "let"
    tellBrackets "{" "}" $ mapM_ tokens ds
    tt "in"
    tokens e
  tokens (ToDo _) = t1 SKOper "?"
  tokens (LamCase _ cs) = do
    tt "fn"
    tellBrackets "{" "}" $ mapM_ tokens cs
  tokens (Case _ e cs) = do
    tt "case"
    tokens e
    tt "of"
    tellBrackets "{" "}" $ mapM_ tokens cs
  tokens (Do _ es) = tt "do" >> tellBrackets "{" "}" (mapM_ tokens es)
  tokens (Lit _ lit) = tokens lit

instance Pretty (Lit t) SyntaxKind where
  -- TODO figure out how to get the arrows to appear infix
  tokens TyFn = tt "(->)"
  tokens TyAuto = t1 SKOper "*"
  tokens TyEmpty = tt "(#?EMPTY?#)"
  tokens KVal = todo
  tokens KMod = todo
  tokens KValFn = todo
  tokens KModFn = todo
  tokens (LInt n) = tt $ show n
  tokens (LFloat (a :% b)) = tellBrackets "(" ")" $ do
    tt $ show a
    t1 SKOper "/"
    tt $ show b
  -- TODO escape properly
  tokens (LString xs) = tt $ "\"" ++ xs ++ "\""
  -- TODO escape properly
  tokens (LChar x) = tt $ "'" ++ [x] ++ "'"

instance Pretty CaseClause SyntaxKind where
  tokens (CaseClause _ pat expr) = do
    tt "if"
    tokens pat
    tt "then"
    tokens expr
    semi

instance Pretty DoElem SyntaxKind where
  tokens (DoLet _ ds) = do
    tt "let"
    tellBrackets "{" "}" (mapM_ tokens ds)
    semi
  tokens (DoBind _ pat expr) = do
    tokens pat
    t1 SKOper "<-"
    tokens expr
    semi
  tokens (DoExpr _ expr) = do
    tokens expr
    semi

instance Pretty Pat SyntaxKind where
  tokens (PatParams _ ps) = mapM_ tokens ps
  tokens (PatBind _ name) = tokens name
  tokens (PatApp _ expr ps) = tellBrackets "(" ")" $ do
    tokens expr
    mapM_ tokens ps
  tokens (PatLit _ lit) = tokens lit
  tokens (PatIgnore _) = tt "_"

