
module Syntax.Pretty where

import Common
import Syntax.Types

instance Pretty No SyntaxKind where
  tokens _ = return ()

instance Pretty Binder SyntaxKind where
  tokens (Binder name Nothing) = tokens name
  tokens (Binder name (Just ty)) = tellBrackets "(" ")" $ do
    tokens name
    colon
    tokens ty

instance (Pretty e SyntaxKind) => Pretty (Binding e) SyntaxKind where
  tokens (Binding (Binder name ty) rhs) = do
    tokens name
    whenJust ty $ \ty -> colon >> tokens ty
    tt "is"
    tokens rhs

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
    when (mode == DataOpen) $ tt "open"
    tokens name
    whenJust parent $ \parent -> t1 SKOper "<:" >> tokens parent
    when (not $ ty == Prim TyEmpty) $ do
      tt "is"
      tokens ty
    case kids of
      [] -> semi
      _  -> tellBrackets "{" "}" $ mapM_ tokens kids
  tokens (Infix assoc prec bs) = do
    tt "infix"
    case assoc of
      InfixLeft -> tt "left"
      InfixRight -> tt "right"
      InfixNone -> return ()
    tt $ show prec
    mapM_ tokens bs
    semi

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
    whenJust bound $ \(TyBound op ty) -> do
      tokens op
      tokens ty
    semi
  tokens (SigMod name ty) = do
    tt "module"
    tokens name
    colon
    tokens ty
    semi

instance Pretty ValDecl SyntaxKind where
  tokens (BindLocalVal b) = tokens b

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
  tokens (EFloat (a :% b)) = tellBrackets "(" ")" $ do
    tt $ show a
    t1 SKOper "/"
    tt $ show b
  -- TODO escape properly
  tokens (EString xs) = tt $ "\"" ++ xs ++ "\""
  -- TODO escape properly
  tokens (EChar x) = tt $ "'" ++ [x] ++ "'"

instance Pretty CaseClause SyntaxKind where
  tokens (CaseClause pat expr) = do
    tt "if"
    tokens pat
    tt "then"
    tokens expr
    semi

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

instance Pretty TyPrim SyntaxKind where
  -- TODO figure out how to get the arrows to appear infix
  tokens TyFn = tt "(->)"
  tokens TyAuto = t1 SKOper "*"
  tokens TyEmpty = tt "(#?EMPTY?#)"

