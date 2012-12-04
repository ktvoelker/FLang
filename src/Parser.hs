
module Parser where

import Text.Parsec hiding (parse)

import qualified Text.Parsec as P

import Common
import Syntax
import Token

type Parser = Parsec [(Token, SourcePos)] ()

parse :: String -> [(Token, SourcePos)] -> FM ModDecl
parse name xs = case P.parse file name xs of
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
  n <- bindName
  t <- optionMaybe hasTy
  return $ Binder n t

modHeader = genModHeader "module"

sigHeader = genModHeader "sig"

modExpr :: Parser ModExpr
modExpr =
  do
    kw "fn"
    ps <- many valParam
    kw "->"
    fmap (Lam ps) modExpr
  <|>
  fmap Record (kw "rec" >> braces (many modDecl))
  <|>
  do
    (x : xs) <- many1 modPrim
    return $ App x xs

modPrim = ref <|> parens modExpr

openMode =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("except", OpenExcept)
    , ("only", OpenOnly)
    ]

openQual = do
  mode <- openMode
  bs <- many1 bindName
  return $ mode bs

dataMode =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("open", DataOpen)
    , ("closed", DataClosed)
    ]

parentTy = kw "<:" >> ty

hasTy = kw ":" >> ty

valParam =
  fmap (flip Binder Nothing) bindName
  <|>
  do
    kw "("
    n <- bindName
    t <- optionMaybe hasTy
    kw ")"
    return $ Binder n t

infixAssoc =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("left", InfixLeft)
    , ("right", InfixRight)
    ]

dataDecl par = do
  o <- optionMaybe dataMode
  n <- bindName
  p <- par
  t <- optionMaybe $ kw "is" >> ty
  c <- (kw ";" >> return []) <|> (braces . many . dataDecl $ return Nothing)
  return $ Data (maybe DataClosed id o) n p (maybe (Prim TyEmpty) id t) c

modDecl =
  fileDecl
  <|>
  do
    kw "open"
    e <- valExpr
    q <- optionMaybe openQual
    kw ";"
    return $ Open e q
  <|>
  do
    kw "val"
    n <- bindName
    t <- optionMaybe hasTy
    kw "is"
    fmap (BindVal . (Binding $ Binder n t)) valPrimEnd
  <|>
  do
    kw "data"
    dataDecl $ optionMaybe parentTy
  <|>
  do
    kw "type"
    n <- bindName
    lhs <- optionMaybe hasTy
    kw "is"
    rhs <- ty
    kw ";"
    return $ BindTy (Binding (Binder n lhs) rhs)
  <|>
  do
    kw "infix"
    a <- optionMaybe infixAssoc
    n <- tok "literal integer" $ \t -> case t of
      TInt n -> Just n
      _ -> Nothing
    bs <- many1 bindName
    kw ";"
    return $ Infix (maybe InfixNone id a) n bs

fileDecl =
  do
    h <- modHeader
    kw "is"
    fmap (BindMod . Binding h) modExpr
  <|>
  do
    h <- sigHeader
    kw "is"
    kw "rec"
    fmap (BindSig . Binding h) . braces . semi $ sigDecl

ident = tok "identifier" $ \t -> case t of
  TId xs -> Just xs
  _ -> Nothing

oper = tok "operator" $ \t -> case t of
  TExprOp xs -> Just xs
  _ -> Nothing

bindName = fmap BindName $ ident <|> parens oper

ref = fmap (Ref . BindName) ident

tyRel = do
  o <- tyCompOp
  t <- ty
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
    n <- bindName
    t <- hasTy
    return $ SigVal n t
  <|>
  do
    kw "type"
    n <- bindName
    t <- optionMaybe tyRel
    return $ SigTy n t
  <|>
  do
    kw "module"
    n <- bindName
    t <- hasTy
    return $ SigMod n t

valExpr = expr "expression" valOp valPrim

valOp = tok "operator" $ \t -> case t of
  TExprOp xs -> Just . Ref . BindName $ xs
  _ -> Nothing

expr descr op prim =
  do
    h <- exprApp prim
    t <- many $ exprOp op prim
    return $ OpChain (Just h) t
  <|>
  do
    t <- many1 $ exprOp op prim
    return $ OpChain Nothing t
  <?>
  descr

exprOp op prim = do
  o <- op
  a <- exprApp prim
  return (o, a)

exprApp prim = do
  h <- exprMember prim
  t <- many $ exprMember prim
  return $ App h t

exprMember prim = do
  h <- prim
  t <- many $ kw "." >> bindName
  return $ foldl Member h t

localBind = do
  n <- bindName
  t <- optionMaybe hasTy
  kw "is"
  e <- valExpr
  return $ BindLocalVal (Binder n t) e

semi = (`sepEndBy1` kw ";")

localBinds = semi localBind

exprLam = kw "fn" >> (lamCase <|> lamPlain)
  where
    lamCase = do
      {--
       - Backtracking is required since the curly brace might be the beginning of
       - an expression rather than a block of clauses.
       --}
      try $ kw "{"
      e <-
        fmap (Prim . LamCase)
        . many1
        $ genCaseClause FnClause [PatIgnore] (many1 pat)
      kw "}"
      return e
    lamPlain = do
      ps <- many valParam
      fmap (Lam ps) valPrimEnd

genCaseClause :: (a -> ValExpr -> b) -> a -> Parser a -> Parser b
genCaseClause con ignore pat =
  do
    kw "if"
    p <- pat
    kw "then"
    fmap (con p) valPrimEnd
  <|>
  do
    kw "else"
    fmap (con ignore) valPrimEnd

exprCase = fmap Prim $ do
  kw "case"
  s <- valExpr
  kw "of"
  cs <- braces . many1 $ genCaseClause CaseClause PatIgnore patApp
  return $ Case s cs

exprRec = fmap Record $ kw "rec" >> braces localBinds

exprBegin = braces valExpr

exprLit = fmap Prim . tok "literal primitive" $ \t -> case t of
  TInt n -> Just $ EInt n
  TFloat n -> Just $ EFloat n
  TString xs -> Just $ EString xs
  TChar c -> Just $ EChar c
  _ -> Nothing

valPrimBlock = choice [exprLam, exprCase, exprRec, exprBegin, exprDo, exprLet]

valPrimEnd = valPrimBlock <|> do
  e <- valExpr
  kw ";"
  return e

valPrim = choice [valPrimBlock, ref, exprLit, kw "?" >> return ToDo, parens valExpr]

exprDo = fmap (Prim . Do) $ kw "do" >> braces (semi doElem)

exprLet = do
  kw "let"
  bs <- localBinds
  kw "in"
  e <- valPrimEnd
  return $ Let bs e

doElem =
  fmap DoLet (kw "let" >> braces localBinds)
  <|>
  do
    p <- patApp
    kw "<-"
    e <- valExpr
    return $ DoBind p e
  <|>
  fmap DoExpr valExpr
  <?>
  "do-element"

pat =
  {--
   - We have to backtrack if this fails because it might have consumed a
   - left parenthesis.
   --}
  try (fmap PatBind bindName)
  <|>
  parens patApp
  <|>
  patLit
  <?>
  "pattern"

patLit = tok "literal primitive pattern" $ \t -> case t of
  TInt n -> Just $ PatInt n
  TString xs -> Just $ PatString xs
  TChar c -> Just $ PatChar c
  _ -> Nothing

patApp = do
  ns <- bindName `sepEndBy1` kw "."
  ps <- many pat
  return $ case ns of
    [] -> error "Impossible!"
    [n] | null ps && namespace n == NsValues -> PatBind n
    (n : ns) -> PatApp (foldl Member (Ref n) ns) ps

ty = do
  qs <- tyQuants
  co <- tyCore
  cs <- many tyConstr
  let
  { qf = case qs of
      [] -> id
      _ -> Lam qs
  }
  return $ qf $ case cs of
    [] -> co
    _ -> Let cs co

tyQuants =
  fmap (maybe [] $ map $ flip Binder Nothing)
  $ optionMaybe
  $ between (kw "forall") (kw ".")
  $ many1 bindName

tyCore = expr "type" tyOp tyPrim

tyOp = kw "->" >> return (Prim TyFn)

tyPrim =
  parens tyCore
  <|>
  (kw "*" >> return (Prim TyAuto))
  <|>
  tyRec
  <|>
  ref

tyRec = fmap Record $ between (kw "rec") (kw "end") $ semi $ do
  n <- bindName
  t <- hasTy
  return $ FieldDecl n t

tyConstr = do
  kw "with"
  lhs <- tyCore
  op <- tyCompOp
  rhs <- tyCore
  return $ Constraint lhs op rhs

