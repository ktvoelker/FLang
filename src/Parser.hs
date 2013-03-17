
module Parser where

import Text.Parsec hiding (parse, (<|>), many, optional)

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
kw xs = tok ("'" ++ xs ++ "'") $ \case
  TKeyword ys | xs == ys -> Just ()
  TId ys | xs == ys -> Just ()
  TExprOp ys | xs == ys -> Just ()
  _ -> Nothing

genModHeader :: String -> Parser (Expr (TyTag k)) -> Parser (Binder k)
genModHeader word hasTyLike =
  pure Binder <* kw word <*> bindName <*> optionMaybe hasTyLike

modHeader :: Parser (Binder Mod)
modHeader = genModHeader "module" hasTy

sigHeader :: Parser (Binder Ty)
sigHeader = genModHeader "sig" hasKind

modExpr :: Parser ModExpr
modExpr = locate $
  pure mkLam <* kw "fn" <*> many valParam <* kw "->" <*> modExpr
  <|>
  pure mkRecord <* kw "rec" <*> braces (many modDecl)
  <|>
  mkApp <$> modPrim <*> many modPrim

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
hasKind = kw ":" >> kind

kind :: Parser KindExpr
kind = todo

valParam :: (TyTag k ~ Ty) => Parser (Binder k)
valParam =
  flip Binder Nothing <$> bindName
  <|>
  pure Binder <* kw "(" <*> bindName <*> optionMaybe hasTy <* kw ")"

infixAssoc :: Parser InfixAssoc
infixAssoc =
  choice
  . map (\(w, e) -> kw w >> return e)
  $ [ ("left", InfixLeft)
    , ("right", InfixRight)
    ]

modDecl :: Parser ModDecl
modDecl = locate $
  fileDecl
  <|>
  pure ((mkBindVal .) . Binding)
    <*  kw "val"
    <*> (Binder <$> bindName <*> optionMaybe hasTy)
    <*  kw "is"
    <*> valPrimEnd
  <|>
  pure mkData
    <*  kw "data"
    <*> (maybe DataClosed id <$> optionMaybe dataMode)
    <*> bindName
    <*> optionMaybe parentTy
    <*> (maybe (mkLit TyEmpty) id <$> optionMaybe (kw "is" >> ty))
    <*  kw ";"
  <|>
  pure ((mkBindTy .) . Binding)
    <*  kw "type"
    <*> (Binder <$> bindName <*> optionMaybe hasKind)
    <*  kw "is"
    <*> ty
    <*  kw ";"
  <|>
  pure mkInfix
    <*  kw "infix"
    <*> (maybe InfixNone id <$> optionMaybe infixAssoc)
    <*> litInt
    <*> many1 bindName
    <*  kw ";"

litInt :: Parser Integer
litInt = tok "literal integer" $ \case
  TInt n -> Just n
  _ -> Nothing

fileDecl :: Parser ModDecl
fileDecl =
  pure ((mkBindMod .) . Binding)
    <*> modHeader
    <*  kw "is"
    <*> modExpr
  <|>
  pure ((mkBindSig .) . Binding)
    <*> sigHeader
    <*  kw "is"
    <*  kw "rec"
    <*> (mkRecord <$> (braces . semi $ sigDecl))

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

ref :: Parser (Expr t)
ref = fmap (mkRef . mkBindName) ident

tyRel :: Parser TyBound
tyRel = mkTyBound <$> tyCompOp <*> ty

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
  pure mkValField <* kw "val" <*> bindName <*> hasTy
  <|>
  pure mkTyField <* kw "type" <*> bindName <*> optionMaybe tyRel
  <|>
  pure mkModField <* kw "module" <*> bindName <*> hasTy

valExpr :: Parser ValExpr
valExpr = expr "expression" valOp valPrim

valOp :: Parser ValExpr
valOp = tok "operator" $ \case
  TExprOp xs -> Just . mkRef . mkBindName $ xs
  _ -> Nothing

expr :: String -> Parser (Expr t) -> Parser (Expr t) -> Parser (Expr t)
expr descr op prim =
  mkOpChain <$> (Just <$> exprApp prim) <*> (many $ exprOp op prim)
  <|>
  mkOpChain Nothing <$> (many1 $ exprOp op prim)
  <?>
  descr

exprOp :: Parser (Expr t) -> Parser (Expr t) -> Parser (Expr t, Expr t)
exprOp op prim = (,) <$> op <*> exprApp prim

exprApp :: Parser (Expr t) -> Parser (Expr t)
exprApp prim = mkApp <$> exprMember prim <*> (many $ exprMember prim)

exprMember :: Parser (Expr t) -> Parser (Expr t)
exprMember prim = mkMember <$> prim <*> (many $ kw "." >> bindName)

localBind =
  pure ((mkBindLocal .) . Binding)
    <*> (Binder <$> bindName <*> optionMaybe hasTy)
    <*  kw "is"
    <*> valExpr

semi = (`sepEndBy1` kw ";")

localBinds = semi localBind

exprLam :: Parser ValExpr
exprLam = kw "fn" >> (lamCase <|> lamPlain)
  where
    lamCase =
      {--
       - Backtracking is required since the curly brace might be the beginning of
       - an expression rather than a block of clauses.
       --}
      pure mkLamCase
        <*  try (kw "{")
        <*> many1 (genCaseClause $ mkPatParams <$> many1 pat)
        <*  kw "}"
    lamPlain = mkLam <$> many valParam <*> valPrimEnd

genCaseClause :: Parser Pat -> Parser CaseClause
genCaseClause pat =
  pure mkCaseClause <* kw "if" <*> pat <* kw "then" <*> valPrimEnd
  <|>
  pure (mkCaseClause mkPatIgnore) <* kw "else" <*> valPrimEnd

exprCase :: Parser ValExpr
exprCase =
  pure mkCase
    <*  kw "case"
    <*> valExpr
    <*  kw "of"
    <*> (braces $ many1 $ genCaseClause patApp)

exprRec :: Parser ValExpr
exprRec = fmap mkRecord $ kw "rec" >> braces localBinds

exprBegin :: Parser ValExpr
exprBegin = braces valExpr

exprLit :: Parser ValExpr
exprLit = fmap mkLit . tok "literal primitive" $ \case
  TInt n -> Just $ LInt n
  TFloat n -> Just $ LFloat n
  TString xs -> Just $ LString xs
  TChar c -> Just $ LChar c
  _ -> Nothing

valPrimBlock :: Parser ValExpr
valPrimBlock = choice [exprLam, exprCase, exprRec, exprBegin, exprDo, exprLet]

valPrimEnd :: Parser ValExpr
valPrimEnd = valPrimBlock <|> valExpr <* kw ";"

valPrim :: Parser ValExpr
valPrim = choice [valPrimBlock, ref, exprLit, kw "?" >> return mkToDo, parens valExpr]

exprDo :: Parser ValExpr
exprDo = fmap mkDo $ kw "do" >> braces (semi doElem)

exprLet :: Parser ValExpr
exprLet = pure mkLet <* kw "let" <*> localBinds <* kw "in" <*> valPrimEnd

doElem :: Parser DoElem
doElem =
  pure mkDoLet <* kw "let" <*> braces localBinds
  <|>
  mkDoBind <$> patApp <* kw "<-" <*> valExpr
  <|>
  mkDoExpr <$> valExpr
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
patLit = fmap mkPatLit $ tok "literal primitive pattern" $ \case
  TInt n -> Just $ LInt n
  TString xs -> Just $ LString xs
  TChar c -> Just $ LChar c
  _ -> Nothing

patName :: Parser Pat
patName = f <$> bindName `sepEndBy1` kw "."
  where
    f [] = error "Impossible!"
    f [n] | namespace n == NsValues = mkPatBind n
    f (n : ns) = mkPatApp (mkMember (mkRef n) ns) []

patApp :: Parser Pat
patApp =
  f <$> bindName `sepEndBy1` kw "." <*> many pat
  where
    f [] _ = error "Impossible!"
    f [n] ps | null ps && namespace n == NsValues = mkPatBind n
    f (n : ns) ps = mkPatApp (mkMember (mkRef n) ns) ps

ty :: Parser TyExpr
ty = (g <$> tyQuants) <*> (f <$> tyCore <*> many tyConstr)
  where
    g [] = id
    g qs = mkLam qs
    f co [] = co
    f co cs = mkLet cs co

tyQuants :: Parser [Binder k]
tyQuants =
  (maybe [] $ map $ flip Binder Nothing)
    <$> (optionMaybe $ between (kw "forall") (kw ".") $ many1 bindName)

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
tyRec =
  mkRecord
    <$> (between (kw "rec") (kw "end") $ semi $ mkValField <$> bindName <*> hasTy)

tyConstr :: Parser TyDecl
tyConstr =
  pure mkConstraint
    <*  kw "with"
    <*> tyCore
    <*> tyCompOp
    <*> tyCore

