
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

genModHeader :: String -> Parser (Expr (TyTag k)) -> Parser (Binder k)
genModHeader word hasTyLike = do
  kw word
  n <- bindName
  Binder n <$> optionMaybe hasTyLike

modHeader :: Parser (Binder Mod)
modHeader = genModHeader "module" hasTy

sigHeader :: Parser (Binder Ty)
sigHeader = genModHeader "sig" hasKind

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

hasKind :: Parser KindExpr
hasKind = todo

valParam :: (TyTag k ~ Ty) => Parser (Binder k)
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
  return $ mkData (maybe DataClosed id o) n p (maybe (mkLit TyEmpty) id t) c

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
    lhs <- optionMaybe hasKind
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

ref :: Parser (Expr k)
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

sigDecl :: Parser TyDecl
sigDecl =
  do
    kw "val"
    n <- bindName
    t <- hasTy
    return $ mkValField n t
  <|>
  do
    kw "type"
    n <- bindName
    t <- optionMaybe tyRel
    return $ mkTyField n t
  <|>
  do
    kw "module"
    n <- bindName
    t <- hasTy
    return $ mkModField n t

valExpr :: Parser ValExpr
valExpr = expr "expression" valOp valPrim

valOp :: Parser ValExpr
valOp = tok "operator" $ \t -> case t of
  TExprOp xs -> Just . mkRef . mkBindName $ xs
  _ -> Nothing

expr :: String -> Parser (Expr k) -> Parser (Expr k) -> Parser (Expr k)
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

exprOp :: Parser (Expr k) -> Parser (Expr k) -> Parser (Expr k, Expr k)
exprOp op prim = do
  o <- op
  a <- exprApp prim
  return (o, a)

exprApp :: Parser (Expr k) -> Parser (Expr k)
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
  return . mkBindLocal $ Binding (Binder n t) e

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
      e <- mkLamCase <$> many1 (genCaseClause $ mkPatParams <$> many1 pat)
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
exprCase = do
  kw "case"
  s <- valExpr
  kw "of"
  fmap (mkCase s) . braces . many1 $ genCaseClause patApp

exprRec :: Parser ValExpr
exprRec = fmap mkRecord $ kw "rec" >> braces localBinds

exprBegin :: Parser ValExpr
exprBegin = braces valExpr

exprLit :: Parser ValExpr
exprLit = fmap mkLit . tok "literal primitive" $ \t -> case t of
  TInt n -> Just $ LInt n
  TFloat n -> Just $ LFloat n
  TString xs -> Just $ LString xs
  TChar c -> Just $ LChar c
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
exprDo = fmap mkDo $ kw "do" >> braces (semi doElem)

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
patLit = fmap mkPatLit $ tok "literal primitive pattern" $ \t -> case t of
  TInt n -> Just $ LInt n
  TString xs -> Just $ LString xs
  TChar c -> Just $ LChar c
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

tyQuants :: Parser [Binder k]
tyQuants =
  fmap (maybe [] $ map $ flip Binder Nothing)
  $ optionMaybe
  $ between (kw "forall") (kw ".")
  $ many1 bindName

tyCore :: Parser TyExpr
tyCore = expr "type" tyOp tyPrim

tyOp :: Parser TyExpr
tyOp = kw "->" >> return (mkLit TyFn)

tyPrim :: Parser TyExpr
tyPrim =
  parens tyCore
  <|>
  (kw "*" >> return (mkLit TyAuto))
  <|>
  tyRec
  <|>
  ref

tyRec :: Parser TyExpr
tyRec = fmap mkRecord $ between (kw "rec") (kw "end") $ semi $ do
  n <- bindName
  t <- hasTy
  return $ mkValField n t

tyConstr :: Parser TyDecl
tyConstr = do
  kw "with"
  lhs <- tyCore
  op <- tyCompOp
  rhs <- tyCore
  return $ mkConstraint lhs op rhs

