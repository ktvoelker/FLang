
module Parser where

import Text.Parsec hiding (parse)

import qualified Text.Parsec as P

import Common
import Syntax
import Token

type Parser = Parsec [(Token, SourcePos)] ()

instance MonadSourcePos Parser where
  getSourcePos = getPosition

parse :: String -> [(Token, SourcePos)] -> FM ModDecl
parse name xs = case P.parse file name xs of
  Left err -> fatal . Err EParser Nothing Nothing . Just . show $ err
  Right decl -> return decl

tok :: String -> (Token -> Maybe a) -> Parser a
tok msg = (<?> msg) . token (show . fst) snd . (. fst)

tokWhen :: String -> (Token -> Bool) -> Parser Token
tokWhen msg pred = tok msg $ \t -> if pred t then Just t else Nothing

tokEq :: Token -> Parser ()
tokEq t = tokWhen (show t) (== t) >> return ()

parens :: Parser a -> Parser a
parens = between (kw "(") (kw ")")

braces :: Parser a -> Parser a
braces = between (kw "{") (kw "}")

file :: Parser ModDecl
file = fileDecl >>= (eof >>) . return

kw :: String -> Parser ()
kw xs = tok ("'" ++ xs ++ "'") $ \t -> case t of
  TKeyword ys | xs == ys -> Just ()
  TId ys | xs == ys -> Just ()
  TExprOp ys | xs == ys -> Just ()
  _ -> Nothing

genModHeader :: String -> Parser Binder
genModHeader word = do
  kw word
  n <- bindName
  Binder n <$> optionMaybe hasTy

modHeader :: Parser Binder
modHeader = genModHeader "module"

sigHeader :: Parser Binder
sigHeader = genModHeader "sig"

modExpr :: Parser ModExpr
modExpr = locate $ do
  do
    kw "fn"
    ps <- many valParam
    kw "->"
    mkLam ps <$> modExpr
  <|>
  fmap mkRecord (kw "rec" >> braces (many modDecl))
  <|>
  do
    (x : xs) <- many1 modPrim
    return $ mkApp x xs

modPrim :: Parser ModExpr
modPrim = ref <|> parens modExpr

dataMode :: Parser DataMode
dataMode =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("open", DataOpen)
    , ("closed", DataClosed)
    ]

parentTy :: Parser TyExpr
parentTy = kw "<:" >> ty

hasTy :: Parser TyExpr
hasTy = kw ":" >> ty

valParam :: Parser Binder
valParam = do
  fmap (flip Binder Nothing) bindName
  <|>
  do
    kw "("
    n <- bindName
    t <- optionMaybe hasTy
    kw ")"
    return $ Binder n t

infixAssoc :: Parser InfixAssoc
infixAssoc =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("left", InfixLeft)
    , ("right", InfixRight)
    ]

dataDecl :: Parser (Maybe TyExpr) -> Parser ModDecl
dataDecl par = do
  o <- optionMaybe dataMode
  n <- bindName
  p <- par
  t <- optionMaybe $ kw "is" >> ty
  c <- (kw ";" >> return []) <|> (braces . many . dataDecl $ return Nothing)
  return $ mkData (maybe DataClosed id o) n p (maybe (mkPrim TyEmpty) id t) c

modDecl :: Parser ModDecl
modDecl = locate $ do
  fileDecl
  <|>
  do
    kw "val"
    n <- bindName
    t <- optionMaybe hasTy
    kw "is"
    fmap (mkBindVal . (Binding $ Binder n t)) valPrimEnd
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
    return $ mkBindTy (Binding (Binder n lhs) rhs)
  <|>
  do
    kw "infix"
    a <- optionMaybe infixAssoc
    n <- tok "literal integer" $ \t -> case t of
      TInt n -> Just n
      _ -> Nothing
    bs <- many1 bindName
    kw ";"
    return $ mkInfix (maybe InfixNone id a) n bs

fileDecl :: Parser ModDecl
fileDecl =
  do
    h <- modHeader
    kw "is"
    fmap (mkBindMod . Binding h) modExpr
  <|>
  do
    h <- sigHeader
    kw "is"
    kw "rec"
    fmap (mkBindSig . Binding h . mkRecord) . braces . semi $ sigDecl

ident :: Parser String
ident = tok "identifier" $ \t -> case t of
  TId xs -> Just xs
  _ -> Nothing

oper :: Parser String
oper = tok "operator" $ \t -> case t of
  TExprOp xs -> Just xs
  _ -> Nothing

bindName :: Parser BindName
bindName = fmap mkBindName $ ident <|> parens oper

ref :: Parser (Expr d e)
ref = fmap (mkRef . mkBindName) ident

tyRel :: Parser TyBound
tyRel = do
  o <- tyCompOp
  t <- ty
  return $ mkTyBound o t

tyCompOp :: Parser TyCompOp
tyCompOp =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("<:", OpSubTy)
    , (">:", OpSuperTy)
    , (":", OpEqualTy)
    ]

sigDecl :: Parser SigDecl
sigDecl =
  do
    kw "val"
    n <- bindName
    t <- hasTy
    return $ mkSigVal n t
  <|>
  do
    kw "type"
    n <- bindName
    t <- optionMaybe tyRel
    return $ mkSigTy n t
  <|>
  do
    kw "module"
    n <- bindName
    t <- hasTy
    return $ mkSigMod n t

valExpr :: Parser ValExpr
valExpr = expr "expression" valOp valPrim

valOp :: Parser ValExpr
valOp = tok "operator" $ \t -> case t of
  TExprOp xs -> Just . mkRef . mkBindName $ xs
  _ -> Nothing

expr :: String -> Parser (Expr d e) -> Parser (Expr d e) -> Parser (Expr d e)
expr descr op prim =
  do
    h <- exprApp prim
    t <- many $ exprOp op prim
    return $ mkOpChain (Just h) t
  <|>
  do
    t <- many1 $ exprOp op prim
    return $ mkOpChain Nothing t
  <?>
  descr

exprOp :: Parser (Expr d e) -> Parser (Expr d e) -> Parser (Expr d e, Expr d e)
exprOp op prim = do
  o <- op
  a <- exprApp prim
  return (o, a)

exprApp :: Parser (Expr d e) -> Parser (Expr d e)
exprApp prim = do
  h <- exprMember prim
  t <- many $ exprMember prim
  return $ mkApp h t

exprMember prim = do
  h <- prim
  t <- many $ kw "." >> bindName
  return $ foldl mkMember h t

localBind = do
  n <- bindName
  t <- optionMaybe hasTy
  kw "is"
  e <- valExpr
  return . mkBindLocalVal $ Binding (Binder n t) e

semi = (`sepEndBy1` kw ";")

localBinds = semi localBind

exprLam :: Parser ValExpr
exprLam = kw "fn" >> (lamCase <|> lamPlain)
  where
    lamCase = do
      {--
       - Backtracking is required since the curly brace might be the beginning of
       - an expression rather than a block of clauses.
       --}
      try $ kw "{"
      e <- mkPrim . mkLamCase <$> many1 (genCaseClause $ mkPatParams <$> many1 pat)
      kw "}"
      return e
    lamPlain = do
      ps <- many valParam
      mkLam ps <$> valPrimEnd

genCaseClause :: Parser Pat -> Parser CaseClause
genCaseClause pat =
  do
    kw "if"
    p <- pat
    kw "then"
    mkCaseClause p <$> valPrimEnd
  <|>
  do
    kw "else"
    mkCaseClause mkPatIgnore <$> valPrimEnd

exprCase :: Parser ValExpr
exprCase = mkPrim <$> do
  kw "case"
  s <- valExpr
  kw "of"
  fmap (mkCase s) . braces . many1 $ genCaseClause patApp

exprRec :: Parser ValExpr
exprRec = fmap mkRecord $ kw "rec" >> braces localBinds

exprBegin :: Parser ValExpr
exprBegin = braces valExpr

exprLit :: Parser ValExpr
exprLit = fmap mkPrim . tok "literal primitive" $ \t -> case t of
  TInt n -> Just $ mkEInt n
  TFloat n -> Just $ mkEFloat n
  TString xs -> Just $ mkEString xs
  TChar c -> Just $ mkEChar c
  _ -> Nothing

valPrimBlock :: Parser ValExpr
valPrimBlock = choice [exprLam, exprCase, exprRec, exprBegin, exprDo, exprLet]

valPrimEnd :: Parser ValExpr
valPrimEnd = valPrimBlock <|> do
  e <- valExpr
  kw ";"
  return e

valPrim :: Parser ValExpr
valPrim = choice [valPrimBlock, ref, exprLit, kw "?" >> return mkToDo, parens valExpr]

exprDo :: Parser ValExpr
exprDo = fmap (mkPrim . mkDo) $ kw "do" >> braces (semi doElem)

exprLet :: Parser ValExpr
exprLet = do
  kw "let"
  bs <- localBinds
  kw "in"
  e <- valPrimEnd
  return $ mkLet bs e

doElem :: Parser DoElem
doElem =
  fmap mkDoLet (kw "let" >> braces localBinds)
  <|>
  do
    p <- patApp
    kw "<-"
    e <- valExpr
    return $ mkDoBind p e
  <|>
  fmap mkDoExpr valExpr
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

patLit :: Parser Pat
patLit = tok "literal primitive pattern" $ \t -> case t of
  TInt n -> Just $ mkPatInt n
  TString xs -> Just $ mkPatString xs
  TChar c -> Just $ mkPatChar c
  _ -> Nothing

patName :: Parser Pat
patName = do
  ns <- bindName `sepEndBy1` kw "."
  return $ case ns of
    [] -> error "Impossible!"
    [n] | namespace n == NsValues -> mkPatBind n
    (n : ns) -> mkPatApp (foldl mkMember (mkRef n) ns) []

patApp :: Parser Pat
patApp = do
  ns <- bindName `sepEndBy1` kw "."
  ps <- many pat
  return $ case ns of
    [] -> error "Impossible!"
    [n] | null ps && namespace n == NsValues -> mkPatBind n
    (n : ns) -> mkPatApp (foldl mkMember (mkRef n) ns) ps

ty :: Parser TyExpr
ty = do
  qs <- tyQuants
  co <- tyCore
  cs <- many tyConstr
  let
  { qf = case qs of
      [] -> id
      _ -> mkLam qs
  }
  return $ qf $ case cs of
    [] -> co
    _ -> mkLet cs co

tyQuants :: Parser [Binder]
tyQuants =
  fmap (maybe [] $ map $ flip Binder Nothing)
  $ optionMaybe
  $ between (kw "forall") (kw ".")
  $ many1 bindName

tyCore :: Parser TyExpr
tyCore = expr "type" tyOp tyPrim

tyOp :: Parser TyExpr
tyOp = kw "->" >> return (mkPrim TyFn)

tyPrim :: Parser TyExpr
tyPrim =
  parens tyCore
  <|>
  (kw "*" >> return (mkPrim TyAuto))
  <|>
  tyRec
  <|>
  ref

tyRec :: Parser TyExpr
tyRec = fmap mkRecord $ between (kw "rec") (kw "end") $ semi $ do
  n <- bindName
  t <- hasTy
  return $ mkFieldDecl n t

tyConstr :: Parser TyDecl
tyConstr = do
  kw "with"
  lhs <- tyCore
  op <- tyCompOp
  rhs <- tyCore
  return $ mkConstraint lhs op rhs

