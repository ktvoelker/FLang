
module Parser where

import Text.Parsec
import Text.Parsec.Combinator

import Common
import Syntax
import Token
import Lexer

type Parser = Parsec [(Token, SourcePos)] ()

parse :: String -> String -> [(Token, SourcePos)]
parse name xs = case parse file name (tokenize name xs) of
  Left err -> error . show $ err
  Right ts -> ts

tok :: (Token -> Maybe a) => Parser a
tok = token (show . fst) snd . (. fst)

tokWhen :: (Token -> Bool) -> Parser Token
tokWhen pred = tok $ \t -> if pred t then Just t else Nothing

tokEq :: Token -> Parser ()
tokEq t = tokWhen (== t) >> return ()

file = (modFile <|> sigFile) eof
  where
    modFile = do
      h <- modHeader
      ds <- decls
      return . BindModule $ h ds
    sigFile = do
      h <- sigHeader
      ds <- sigDecls
      return . BindSig $ h ds

kw = tok $ \t -> case t of
  TKeyword xs -> Just xs
  _ -> Nothing

{--
  
  def genModuleHeader(word: String, ty: UTy.Expr): Parser[Binder] = kw(word) ~> bindName ~ opt(hasType) ^^ Binder;
    
  def moduleHeader = genModuleHeader("module", TyAuto);
  
  def sigHeader = genModuleHeader("sig", TyAuto);
  
  def moduleExpr: Parser[UMod.Expr] =
    kw("fn") ~> ((rep(valParam) <~ kw("->")) ~ moduleExpr ^^ UMod.Lam) |
    modulePrim ~ rep(modulePrim) ^^ UMod.App;

  def modulePrim: Parser[UMod.Expr] = ref[UMod.type](UMod) | kw("(") ~> moduleExpr <~ kw(")");
  
  def openQual: Parser[OpenQual] =
    ((kw("except") | kw("only")) ^^ (_ === "only")) ~ rep1(bindName) ^^ OpenQual;
  
  def decls: Parser[UMod.Expr] = rep(decl) ^^ UMod.Record;
  
  def sigDecls: Parser[USig.Expr] = rep(sigDecl) ^^ USig.Record;
  
  def decl: Parser[ModDecl] =
    kw("open") ~> moduleExpr ~ opt(openQual) ^^ Open |
    (kw("val") ~> (bindName ~ opt(hasType) ^^ Binder) <~ kw("is")) ~ expr(exprPrim) ^^ BindVal |
    kw("data") ~> (dataOpen ~ bindName ~ opt(parentType) <~ kw("is")) ~ ty ^^ Data |
    kw("type") ~> ((bindName ~ opt(hasType) ^^ Binder) <~ kw("is")) ~ ty ^^ BindType |
    (moduleHeader <~ kw("is")) ~ (moduleExpr | decls <~ kw("end")) ^^ BindModule |
    (sigHeader <~ kw("is")) ~ sigDecls <~ kw("end") ^^ BindSig |
    kw("infix") ~> infixAssoc ~ (tok[TInt] ^^ (x => x.n)) ~ rep1(bindName) ^^ Infix;
  
  def bindName: Parser[BindName] = (tok[TId] ^^ (x => x.xs) | tok[TExprOp] ^^ (x => x.xs)) ^^ BindName;
  
  def dataOpen: Parser[Boolean] =
    kw("open") ~> success(true) |
    kw("closed") ~> success(false) |
    success(false);
    
  def parentType: Parser[UTy.Expr] = kw("<:") ~> ty;

  def infixAssoc: Parser[InfixAssoc] =
    kw("left") ~> success(InfixLeft) |
    kw("right") ~> success(InfixRight) |
    success(InfixNone);
    
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
  
  def ref[U <: Universe](implicit u: U): Parser[U#Ref] =
    tok[TId] ^^ (t => u.Ref(BindName(t.xs)));

  def valParam: Parser[Binder] =
    bindName ^^ (x => Binder(x, None)) |
    kw("(") ~> (bindName ~ opt(hasType) ^^ Binder) <~ kw(")");
  
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
