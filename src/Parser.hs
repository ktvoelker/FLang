
module Parser where

import Text.Parsec hiding (parse)

import qualified Text.Parsec as P

import Common
import Syntax
import Token

type Parser = Parsec [(Token, SourcePos)] ()

type LParser a = Parser (L a)

locate :: Parser a -> LParser a
locate parser = flip L <$> getPosition <*> parser

parse :: String -> [(Token, SourcePos)] -> FM (L ModDecl)
parse name xs = case P.parse (locate file) name xs of
  Left err -> fatal $ EParser err
  Right decl -> return decl

tok :: String -> (Token -> Maybe a) -> Parser a
tok msg = (<?> msg) . token (show . fst) snd . (. fst)

tokWhen :: String -> (Token -> Bool) -> Parser Token
tokWhen msg pred = tok msg $ \t -> if pred t then Just t else Nothing

tokEq :: Token -> Parser ()
tokEq t = tokWhen (show t) (== t) >> return ()

parens = between (kw "(") (kw ")")

braces = between (kw "{") (kw "}")

file = fileDecl >>= (eof >>) . return

kw xs = tok ("'" ++ xs ++ "'") $ \t -> case t of
  TKeyword ys | xs == ys -> Just ()
  TId ys | xs == ys -> Just ()
  TExprOp ys | xs == ys -> Just ()
  _ -> Nothing

genModHeader word = do
  kw word
  n <- locate bindName
  t <- optionMaybe hasTy
  return $ Binder n t

modHeader = genModHeader "module"

sigHeader = genModHeader "sig"

modExpr :: LParser ModExpr
modExpr = locate $ do
  do
    kw "fn"
    ps <- many $ locate valParam
    kw "->"
    Lam ps <$> modExpr
  <|>
  fmap Record (kw "rec" >> braces (many modDecl))
  <|>
  do
    (x : xs) <- many1 $ locate modPrim
    return $ App x xs

modPrim = ref <|> (lVal <$> parens modExpr)

dataMode =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("open", DataOpen)
    , ("closed", DataClosed)
    ]

parentTy = kw "<:" >> locate ty

hasTy = kw ":" >> locate ty

valParam =
  fmap (flip Binder Nothing) (locate bindName)
  <|>
  do
    kw "("
    n <- locate bindName
    t <- optionMaybe hasTy
    kw ")"
    return $ Binder n t

infixAssoc =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("left", InfixLeft)
    , ("right", InfixRight)
    ]

dataDecl :: Parser (Maybe (L TyExpr)) -> Parser ModDecl
dataDecl par = do
  o <- optionMaybe $ locate dataMode
  n <- locate bindName
  p <- par
  t <- optionMaybe $ kw "is" >> locate ty
  c <- (kw ";" >> return []) <|> (braces . many . locate . dataDecl $ return Nothing)
  return $ Data
    (maybe (L DataClosed noLoc) id o) n p
    (maybe (L (Prim TyEmpty) noLoc) id t) c

modDecl = locate $ do
  fileDecl
  <|>
  do
    kw "val"
    n <- locate bindName
    t <- optionMaybe hasTy
    kw "is"
    BindVal . (Binding $ Binder n t) <$> locate valPrimEnd
  <|>
  do
    kw "data"
    dataDecl $ optionMaybe parentTy
  <|>
  do
    kw "type"
    n <- locate bindName
    lhs <- optionMaybe hasTy
    kw "is"
    rhs <- locate ty
    kw ";"
    return $ BindTy (Binding (Binder n lhs) rhs)
  <|>
  do
    kw "infix"
    a <- optionMaybe $ locate infixAssoc
    n <- locate $ tok "literal integer" $ \t -> case t of
      TInt n -> Just n
      _ -> Nothing
    bs <- many1 $ locate bindName
    kw ";"
    return $ Infix (maybe (L InfixNone noLoc) id a) n bs

fileDecl =
  do
    h <- modHeader
    kw "is"
    BindMod . Binding h <$> modExpr
  <|>
  do
    h <- sigHeader
    kw "is"
    kw "rec"
    rec <- locate (Record <$> (braces . semi . locate $ sigDecl))
    return $ BindSig $ Binding h rec

ident = tok "identifier" $ \t -> case t of
  TId xs -> Just xs
  _ -> Nothing

oper = tok "operator" $ \t -> case t of
  TExprOp xs -> Just xs
  _ -> Nothing

bindName = fmap BindName $ ident <|> parens oper

ref = Ref <$> (locate $ BindName <$> ident)

tyRel = do
  o <- tyCompOp
  t <- locate ty
  return $ TyBound o t

tyCompOp =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("<:", OpSubTy)
    , (">:", OpSuperTy)
    , (":", OpEqualTy)
    ]

sigDecl =
  do
    kw "val"
    n <- locate bindName
    t <- hasTy
    return $ SigVal n t
  <|>
  do
    kw "type"
    n <- locate bindName
    t <- optionMaybe $ locate tyRel
    return $ SigTy n t
  <|>
  do
    kw "module"
    n <- locate bindName
    t <- hasTy
    return $ SigMod n t

valExpr :: Parser ValExpr
valExpr = expr "expression" valOp valPrim

valOp = do
  pos <- getPosition
  tok "operator" $ \t -> case t of
    TExprOp xs -> Just . Ref . flip L pos . BindName $ xs
    _ -> Nothing

expr descr op prim =
  do
    h <- locate $ exprApp prim
    t <- many $ exprOp op prim
    return $ OpChain (Just h) t
  <|>
  do
    t <- many1 $ exprOp op prim
    return $ OpChain Nothing t
  <?>
  descr

exprOp op prim = do
  o <- locate op
  a <- locate $ exprApp prim
  return (o, a)

exprApp prim = do
  h <- exprMember prim
  t <- many $ exprMember prim
  return $ App h t

exprMember :: Parser (Expr d e) -> LParser (Expr d e)
exprMember prim = do
  loc <- getPosition
  h <- locate prim
  t <- many $ kw "." >> locate bindName
  return $ foldl (\h t -> L (Member h t) loc) h t

localBind = do
  n <- locate bindName
  t <- optionMaybe hasTy
  kw "is"
  e <- locate valExpr
  return . BindLocalVal $ Binding (Binder n t) e

semi = (`sepEndBy1` kw ";")

localBinds = semi $ locate localBind

exprLam = kw "fn" >> (lamCase <|> lamPlain)
  where
    lamCase = do
      {--
       - Backtracking is required since the curly brace might be the beginning of
       - an expression rather than a block of clauses.
       --}
      try $ kw "{"
      e <- Prim . LamCase
        <$> many1 (locate . genCaseClause $ PatParams <$> many1 (locate pat))
      kw "}"
      return e
    lamPlain = do
      ps <- many $ locate valParam
      Lam ps <$> locate valPrimEnd

genCaseClause :: Parser Pat -> Parser CaseClause
genCaseClause pat =
  do
    kw "if"
    p <- pat
    kw "then"
    CaseClause p <$> valPrimEnd
  <|>
  do
    kw "else"
    CaseClause PatIgnore <$> valPrimEnd

exprCase = fmap Prim $ do
  kw "case"
  s <- locate valExpr
  kw "of"
  cs <- braces . many1 . locate $ genCaseClause patApp
  return $ Case s cs

exprRec = fmap Record $ kw "rec" >> braces localBinds

exprBegin = braces valExpr

exprLit = do
  pos <- getPosition
  fmap Prim . tok "literal primitive" $ \t -> case t of
    TInt n -> Just $ EInt $ L n pos
    TFloat n -> Just $ EFloat $ L n pos
    TString xs -> Just $ EString $ L xs pos
    TChar c -> Just $ EChar $ L c pos
    _ -> Nothing

valPrimBlock :: Parser ValExpr
valPrimBlock = choice [exprLam, exprCase, exprRec, exprBegin, exprDo, exprLet]

valPrimEnd :: Parser ValExpr
valPrimEnd = valPrimBlock <|> do
  e <- valExpr
  kw ";"
  return e

valPrim = choice [valPrimBlock, ref, exprLit, kw "?" >> return ToDo, parens valExpr]

exprDo = fmap (Prim . Do) $ kw "do" >> braces (semi $ locate doElem)

exprLet = do
  kw "let"
  bs <- localBinds
  kw "in"
  Let bs <$> locate valPrimEnd

doElem =
  fmap DoLet (kw "let" >> braces localBinds)
  <|>
  do
    p <- locate patApp
    kw "<-"
    DoBind p <$> locate valExpr
  <|>
  DoExpr <$> locate valExpr
  <?>
  "do-element"

pat :: Parser Pat
pat =
  {--
   - We have to backtrack if this fails because it might have consumed a
   - left parenthesis.
   --}
  try patName
  <|>
  parens patApp
  <|>
  patLit
  <?>
  "pattern"

patLit = do
  pos <- getPosition
  tok "literal primitive pattern" $ \t -> case t of
    TInt n -> Just $ PatInt $ L n pos
    TString xs -> Just $ PatString $ L xs pos
    TChar c -> Just $ PatChar $ L c pos
    _ -> Nothing

patName = do
  pos <- getPosition
  ns <- locate bindName `sepEndBy1` kw "."
  return $ case ns of
    [] -> error "Impossible!"
    [n] | namespace (lVal n) == NsValues -> PatBind n
    (n : ns) -> PatApp (foldl (\h t -> L (Member h t) pos) (L (Ref n) pos) ns) []

patApp = do
  pos <- getPosition
  ns <- locate bindName `sepEndBy1` kw "."
  ps <- many $ locate pat
  return $ case ns of
    [] -> error "Impossible!"
    [n] | null ps && namespace (lVal n) == NsValues -> PatBind n
    (n : ns) -> PatApp (foldl (\h t -> L (Member h t) pos) (L (Ref n) pos) ns) ps

ty :: Parser TyExpr
ty = do
  qs <- tyQuants
  co <- locate tyCore
  cs <- many $ locate tyConstr
  let
  { qf = case qs of
      [] -> lVal
      _ -> Lam qs
  }
  return $ qf $ case cs of
    [] -> co
    _ -> L (Let cs co) $ lLoc co

tyQuants :: Parser [L Binder]
tyQuants =
  fmap (maybe [] $ map $ \n@(L _ loc) -> L (Binder n Nothing) loc)
  $ optionMaybe
  $ between (kw "forall") (kw ".")
  $ many1
  $ locate bindName

tyCore :: Parser TyExpr
tyCore = expr "type" tyOp tyPrim

tyOp :: Parser TyExpr
tyOp = kw "->" >> return (Prim TyFn)

tyPrim :: Parser TyExpr
tyPrim =
  parens tyCore
  <|>
  (kw "*" >> return (Prim TyAuto))
  <|>
  tyRec
  <|>
  ref

tyRec :: Parser TyExpr
tyRec = fmap Record $ between (kw "rec") (kw "end") $ semi $ do
  n <- locate bindName
  t <- hasTy
  return $ L (FieldDecl n t) $ lLoc n

tyConstr :: Parser TyDecl
tyConstr = do
  kw "with"
  lhs <- locate tyCore
  op <- tyCompOp
  rhs <- locate tyCore
  return $ Constraint lhs op rhs

