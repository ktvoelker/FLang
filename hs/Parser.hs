
module Parser where

import Text.Parsec hiding (parse)

import qualified Text.Parsec as P

import Syntax
import Token
import Lexer

type Parser = Parsec [(Token, SourcePos)] ()

parse :: String -> String -> ModDecl
parse name xs = case P.parse file name (tokenize name xs) of
  Left err -> error . show $ err
  Right ts -> ts

tok :: String -> (Token -> Maybe a) -> Parser a
tok msg = (<?> msg) . token (show . fst) snd . (. fst)

tokWhen :: String -> (Token -> Bool) -> Parser Token
tokWhen msg pred = tok msg $ \t -> if pred t then Just t else Nothing

tokEq :: Token -> Parser ()
tokEq t = tokWhen (show t) (== t) >> return ()

parens = between (kw "(") (kw ")")

file = fileDecl >>= (eof >>) . return

kw xs = tok ("'" ++ xs ++ "'") $ \t -> case t of
  TKeyword ys | xs == ys -> Just ()
  TId ys | xs == ys -> Just ()
  TExprOp ys | xs == ys -> Just ()
  _ -> Nothing

genModHeader word = do
  kw word
  n <- bindName
  t <- optionMaybe hasType
  return $ Binder n t

modHeader = genModHeader "module"

sigHeader = genModHeader "sig"

modExpr :: Parser ModExpr
modExpr =
  do
    kw "fn"
    ps <- many valParam
    kw "->"
    e <- modExpr
    kw "end"
    return $ Lam ps e
  <|>
  fmap Record (between (kw "rec") (kw "end") $ many modDecl)
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

parentType = kw "<:" >> ty

hasType = kw ":" >> ty

valParam =
  fmap (flip Binder Nothing) bindName
  <|>
  do
    kw "("
    n <- bindName
    t <- optionMaybe hasType
    kw ")"
    return $ Binder n t

infixAssoc =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("left", InfixLeft)
    , ("right", InfixRight)
    ]

modDecl =
  fileDecl
  <|>
  do
    kw "open"
    e <- valExpr
    q <- optionMaybe openQual
    return $ Open e q
  <|>
  do
    kw "val"
    n <- bindName
    t <- optionMaybe hasType
    kw "is"
    e <- valExpr
    return $ BindVal (Binder n t) e
  <|>
  do
    a <- optionMaybe $ kw "empty"
    kw "data"
    o <- optionMaybe dataMode
    n <- bindName
    p <- optionMaybe parentType
    t <- case a of
      Nothing -> kw "is" >> ty
      Just _ -> return $ Prim TyEmpty
    return $ Data (maybe DataClosed id o) n p t
  <|>
  do
    kw "type"
    n <- bindName
    lhs <- optionMaybe hasType
    kw "is"
    rhs <- ty
    return $ BindType (Binder n lhs) rhs
  <|>
  do
    kw "infix"
    a <- optionMaybe infixAssoc
    n <- tok "literal integer" $ \t -> case t of
      TInt n -> Just n
      _ -> Nothing
    bs <- many1 bindName
    return $ Infix (maybe InfixNone id a) n bs

fileDecl =
  do
    h <- modHeader
    kw "is"
    fmap (BindModule h) modExpr
  <|>
  do
    h <- sigHeader
    kw "is"
    kw "rec"
    ds <- sigDecls
    kw "end"
    return $ BindSig h ds

ident = tok "identifier" $ \t -> case t of
  TId xs -> Just xs
  _ -> Nothing

oper = tok "operator" $ \t -> case t of
  TExprOp xs -> Just xs
  _ -> Nothing

bindName = fmap BindName $ ident <|> parens oper

ref = fmap (Ref . BindName) ident

sigDecls = many sigDecl

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
    t <- hasType
    return $ SigVal n t
  <|>
  do
    kw "type"
    n <- bindName
    t <- optionMaybe tyRel
    return $ SigType n t
  <|>
  do
    kw "module"
    n <- bindName
    t <- hasType
    return $ SigModule n t

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
  t <- optionMaybe hasType
  kw "is"
  e <- valExpr
  return $ BindLocalVal (Binder n t) e

semi = (`sepEndBy1` kw ";")

localBinds = semi localBind

exprEnd = do
  bs <- optionMaybe $ kw "where" >> localBinds
  kw "end"
  return bs

withWhere p = do
  r <- p
  bs <- exprEnd
  return $ case bs of
    Nothing -> r
    Just bs -> Let bs r

exprLam = kw "fn" >> (lamPlain <|> lamCase)
  where
    lamPlain = do
      ps <- many valParam
      kw "->"
      e <- valExpr
      return $ Lam ps e
    lamCase = fmap Prim $ do
      cs <- many1 fnClause
      return $ LamCase cs

genCaseClause :: (a -> ValExpr -> b) -> a -> Parser a -> Parser b
genCaseClause con ignore pat =
  do
    kw "if"
    p <- pat
    kw "then"
    e <- valExpr
    return $ con p e
  <|>
  do
    kw "else"
    e <- valExpr
    return $ con ignore e

fnClause = genCaseClause FnClause [PatIgnore] $ many1 pat

exprCase = fmap Prim $ do
  kw "case"
  s <- valExpr
  cs <- many1 caseClause
  return $ Case s cs

caseClause = genCaseClause CaseClause PatIgnore patApp

exprRec = fmap Record $ kw "rec" >> localBinds

exprBegin = kw "begin" >> valExpr

exprLit = fmap Prim . tok "literal primitive" $ \t -> case t of
  TInt n -> Just $ EInt n
  TFloat n -> Just $ EFloat n
  TString xs -> Just $ EString xs
  TChar c -> Just $ EChar c
  _ -> Nothing

choiceWithWhere = choice . map withWhere

valPrimDo =
  choiceWithWhere [exprLam, exprCase, exprRec, exprBegin]
  <|>
  ref
  <|>
  exprLit
  <|>
  (kw "?" >> return ToDo)

exprDo = fmap (Prim . Do) $ kw "do" >> semi doElem

exprLet = do
  kw "let"
  bs <- localBinds
  kw "in"
  e <- valExpr
  return $ Let bs e

valPrim =
  valPrimDo
  <|>
  choiceWithWhere [exprDo, exprLet]
  <|>
  parens valExpr

doElem =
  fmap DoLet (between (kw "let") (kw "end") localBinds)
  <|>
  do
    p <- patApp
    kw "<-"
    e <- valExpr
    return $ DoBind p e
  <|>
  fmap DoExpr (expr "expression" valOp valPrimDo)
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
  t <- hasType
  return $ FieldDecl n t

tyConstr = do
  kw "with"
  lhs <- tyCore
  op <- tyCompOp
  rhs <- tyCore
  return $ Constraint lhs op rhs

