
module Parser where

import Text.Parsec hiding (parse)
import Text.Parsec.Combinator

import qualified Text.Parsec as P

import Common
import Syntax
import Token
import Lexer

type Parser = Parsec [(Token, SourcePos)] ()

parse :: String -> String -> ModDecl
parse name xs = case P.parse file name (tokenize name xs) of
  Left err -> error . show $ err
  Right ts -> ts

tok :: (Token -> Maybe a) -> Parser a
tok = token (show . fst) snd . (. fst)

tokWhen :: (Token -> Bool) -> Parser Token
tokWhen pred = tok $ \t -> if pred t then Just t else Nothing

tokEq :: Token -> Parser ()
tokEq t = tokWhen (== t) >> return ()

file = (modFile <|> sigFile) >>= \f -> eof >> return f
  where
    modFile = do
      h <- modHeader
      ds <- modDecls
      return $ BindModule h ds
    sigFile = do
      h <- sigHeader
      ds <- sigDecls
      return $ BindSig h ds

kw xs = tok $ \t -> case t of
  TKeyword ys | xs == ys -> Just ys
  _ -> Nothing

genModHeader word = do
  kw word
  n <- bindName
  t <- optionMaybe hasType
  return $ Binder n t

modHeader = genModHeader "module"

sigHeader = genModHeader "sig"

modExpr =
  do
    kw "fn"
    ps <- many valParam
    kw "->"
    e <- modExpr
    return $ Lam ps e
  <|> do
    (x : xs) <- many1 modPrim
    return $ App x xs

modPrim = ref <|> between (kw "(") (kw ")") modExpr

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

modDecls = fmap Record . many $ modDecl

modDecl =
  do
    kw "open"
    e <- modExpr
    q <- optionMaybe openQual
    return $ Open e q
  <|>
  do
    kw "val"
    n <- bindName
    t <- optionMaybe hasType
    kw "is"
    e <- expr exprPrim
    return $ BindVal (Binder n t) e
  <|>
  do
    kw "data"
    o <- optionMaybe dataMode
    n <- bindName
    p <- optionMaybe parentType
    kw "is"
    t <- ty
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
    h <- modHeader
    kw "is"
    e <- modExpr <|> (modDecls >>= \ds -> kw "end" >> return ds)
    return $ BindModule h e
  <|>
  do
    h <- sigHeader
    kw "is"
    ds <- sigDecls
    kw "end"
    return $ BindSig h ds
  <|>
  do
    kw "infix"
    a <- optionMaybe infixAssoc
    n <- tok $ \t -> case t of
      TInt n -> Just n
      _ -> Nothing
    bs <- many1 bindName
    return $ Infix (maybe InfixNone id a) n bs

bindName = fmap BindName $ tok $ \t -> case t of
  TId xs -> Just xs
  TExprOp xs -> Just xs
  _ -> Nothing

ref = fmap (Ref . BindName) $ tok $ \t -> case t of
  TId xs -> Just xs
  _ -> Nothing

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

expr prim =
  do
    h <- exprApp prim
    t <- many $ exprOp prim
    return $ OpChain (Just h) t
  <|>
  do
    t <- many1 $ exprOp prim
    return $ OpChain Nothing t

exprApp prim = do
  h <- prim
  t <- many prim
  return $ App h t

exprOp prim = do
  o <- tok $ \t -> case t of
    TExprOp xs -> Just . Ref . BindName $ xs
    _ -> Nothing
  a <- exprApp prim
  return (o, a)

localBind = do
  n <- bindName
  t <- optionMaybe hasType
  kw "is"
  e <- expr exprPrim
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
      e <- expr exprPrim
      return $ Lam ps e
    lamCase = fmap Prim $ do
      kw "of"
      cs <- semi fnClause
      return $ LamCase cs

fnClause = do
  ps <- many1 pat
  kw "->"
  e <- expr exprPrim
  return $ FnClause ps e

exprCase = fmap Prim $ do
  kw "case"
  s <- expr exprPrim
  kw "of"
  cs <- semi caseClause
  return $ Case s cs

caseClause = do
  p <- patApp
  kw "->"
  e <- expr exprPrim
  return $ CaseClause p e

exprRec = fmap Record $ kw "rec" >> localBinds

exprBegin = kw "begin" >> expr exprPrim

exprLit = fmap Prim . tok $ \t -> case t of
  TInt n -> Just $ EInt n
  TFloat n -> Just $ EFloat n
  TString xs -> Just $ EString xs
  TChar c -> Just $ EChar c
  _ -> Nothing

choiceWithWhere = choice . map withWhere

exprPrimDo =
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
  e <- expr exprPrim
  return $ Let bs e

exprPrim =
  exprPrimDo
  <|>
  choiceWithWhere [exprDo, exprLet]
  <|>
  between (kw "(") (kw ")") (expr exprPrim)

doElem =
  fmap DoLet (between (kw "let") (kw "end") localBinds)
  <|>
  do
    p <- patApp
    kw "<-"
    e <- expr exprPrim
    return $ DoBind p e
  <|>
  fmap DoExpr (expr exprPrimDo)

pat =
  fmap PatBind bindName
  <|>
  between (kw "(") (kw ")") patApp
  <|>
  patLit

patLit = tok $ \t -> case t of
  TInt n -> Just $ PatInt n
  TString xs -> Just $ PatString xs
  TChar c -> Just $ PatChar c
  _ -> Nothing

patApp = do
  ns <- bindName `sepEndBy1` kw "."
  ps <- many pat
  return $ case ns of
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

tyCore = do
  h <- tyApp
  t <- many $ kw "->" >> tyApp
  return $ OpChain (Just h) $ map (Prim TyFn,) t

tyApp = do
  h <- tyPrim
  t <- many tyPrim
  return $ App h t

tyPrim =
  between (kw "(") (kw ")") tyCore
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

