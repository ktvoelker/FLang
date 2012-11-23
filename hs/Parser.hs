
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

sigDecls = fmap Record . many $ sigDecl

sigDecl = undefined

ty = undefined

expr = undefined

exprPrim = undefined

{--
  def sigDecl: Parser[SigDecl] =
    kw("val") ~> bindName ~ hasType ^^ SigVal |
    kw("type") ~> bindName ~ opt(typeRel) ^^ SigType |
    kw("module") ~> bindName ~ hasType ^^ SigModule;
    
  def tyCompOp: Parser[TyCompOp] =
    kw("<:") ~> success(OpSubTy) |
    kw("<:") ~> success(OpSuperTy) |
    kw(":") ~> success(OpEqualTy);
    
  def typeRel: Parser[TyBound] = tyCompOp ~ ty ^^ TyBound;
  
  def expr(prim: Parser[UVal.Expr]): Parser[UVal.Expr] =
    (exprApp(prim) ^^ Some.apply) ~ rep(exprOp(prim)) ^^ UVal.OpChain |
    rep1(exprOp(prim)) ^^ (xs => UVal.OpChain(None, xs));
  
  def exprApp(prim: Parser[UVal.Expr]): Parser[UVal.Expr] =
    prim ~ rep(prim) ^^ UVal.App;
  
  def exprOp(prim: Parser[UVal.Expr]): Parser[(UVal.Expr, UVal.Expr)] =
    (tok[TExprOp] ^^ (x => UVal.Ref(BindName(x.xs)))) ~ exprApp(prim) ^^
    (x => (x._1, x._2));
  
  def localBind: Parser[ValDecl] =
      (bindName ~ opt(hasType) <~ kw("is") ^^ Binder) ~ expr(exprPrim) ^^ BindLocalVal;
  
  def semi[T](p: Parser[T]): Parser[List[T]] = rep1sep(p, kw(";")) <~ opt(kw(";"));
  
  def localBinds: Parser[List[ValDecl]] = semi(localBind);
  
  def exprEnd: Parser[List[ValDecl]] =
    (opt(kw("where") ~> localBinds) ^^ (x => x.getOrElse(Nil))) <~ kw("end");
  
  def withLocalBinds(e: UVal.Expr, bs: List[ValDecl]) = e.withLocalBinds(bs);
  
  def exprPrimDo: Parser[UVal.Expr] =
    kw("fn") ~> ((rep(valParam) <~ kw("->")) ~ expr(exprPrim) ^^ UVal.Lam) ~
      exprEnd ^^ withLocalBinds |
    kw("fn") ~> kw("of") ~> (semi(fnClause) ^^ LamCase) ~ exprEnd ^^ withLocalBinds |
    kw("case") ~> ((expr(exprPrim) <~ kw("of")) ~ semi(caseClause) ^^ Case) ~
      exprEnd ^^ withLocalBinds |
    kw("rec") ~> (localBinds ^^ UVal.Record) ~ exprEnd ^^ withLocalBinds |
    kw("begin") ~> expr(exprPrim) ~ exprEnd ^^ withLocalBinds |
    ref[UVal.type](UVal) |
    tok[TInt] ^^ (t => EInt(t.n)) |
    tok[TFloat] ^^ (t => EFloat(t.n)) |
    tok[TString] ^^ (t => EString(t.xs)) |
    tok[TChar] ^^ (t => EChar(t.char)) |
    kw("?") ~> success(UVal.ToDo);
  
  def exprPrim: Parser[UVal.Expr] =
    exprPrimDo |
    kw("do") ~> (semi(doElem) ^^ Do) ~ exprEnd ^^ withLocalBinds |
    kw("let") ~> ((localBinds <~ kw("in")) ~ expr(exprPrim) ^^ UVal.Let) ~
      exprEnd ^^ withLocalBinds |
    kw("(") ~> expr(exprPrim) <~ kw(")");
  
  def fnClause: Parser[LamCaseClause] = (rep1(pat) <~ kw("->")) ~ expr(exprPrim) ^^ LamCaseClause;
  
  def caseClause: Parser[CaseClause] = (patApp <~ kw("->")) ~ expr(exprPrim) ^^ CaseClause;
  
  def doElem: Parser[DoElem] =
    kw("let") ~> localBinds <~ kw("end") ^^ DoLet |
    (patApp <~ kw("<-")) ~ expr(exprPrim) ^^ DoBind |
    expr(exprPrimDo) ^^ DoExpr;
  
  def pat: Parser[Pat] =
    bindName ^^ PatBind |
    kw("(") ~> patApp <~ kw(")") |
    tok[TInt] ^^ (t => PatInt(t.n)) |
    tok[TString] ^^ (t => PatString(t.xs)) |
    tok[TChar] ^^ (t => PatChar(t.char));

  def name = rep1sep(tok[TId] ^^ (t => BindName(t.xs)), kw("."));
  
  def patApp: Parser[Pat] = name ~ rep(pat) ^^
      (x => if (x._1.length === 1 && x._1.head.namespace === NsValues) {
        PatBind(x._1.head)
      } else {
        PatApp(
          x._1.tail.foldLeft[UVal.Expr](UVal.Ref(x._1.head))(UVal.Member.apply),
          x._2)
      });
  
  def hasType: Parser[UTy.Expr] = kw(":") ~> ty;
  
  def ty: Parser[UTy.Expr] = tyQuants ~ (tyCore ~ rep(tyConstr) ^^ flip ^^ UTy.Let) ^^ UTy.Lam;
  
  def tyQuants: Parser[List[Binder]] =
    opt(kw("forall") ~> rep1(bindName ^^ {Binder(_, None)}) <~ kw(".")) ^^ (m => m.getOrElse(Nil));
  
  def tyCore: Parser[UTy.Expr] =
    (tyApp ^^ Some.apply) ~ rep(kw("->") ~> tyApp ^^ (x => (TyFn, x))) ^^ UTy.OpChain;
  
  def tyApp: Parser[UTy.Expr] = tyPrim ~ rep(tyPrim) ^^ UTy.App;
  
  def tyPrim: Parser[UTy.Expr] =
    kw("(") ~> tyCore <~ kw(")") |
    kw("*") ~> success(TyAuto) |
    kw("rec") ~> semi(bindName ~ hasType ^^ FieldDecl) <~ kw("end") ^^ UTy.Record |
    ref[UTy.type](UTy);
  
  def tyConstr: Parser[Constraint] = kw("with") ~> tyCore ~ tyCompOp ~ tyCore ^^ Constraint;

--}
