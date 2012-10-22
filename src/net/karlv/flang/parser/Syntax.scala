package net.karlv.flang.parser

import net.karlv.flang.ast._
import java.io.InputStream
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.token
import scala.util.parsing.input.Reader
import java.io.InputStreamReader
import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq

object Syntax extends TokenParsers with ImplicitConversions {
  
  override type Tokens = token.Tokens;
  
  override val lexical = Lexicon;
  
  def apply(is: InputStream): FileBind = this(Lexicon(is));

  def apply(ts: Reader[Lexicon.Token]): FileBind = start(ts) match {
    case Success(tree, _) => tree;
    case failure: NoSuccess => sys.error(failure.toString());
  };
  
  def start: Parser[FileBind] = phrase(file);
  
  def file: Parser[FileBind] = moduleHeader ~ decls ^^ BindModule | sigHeader ~ sigDecls ^^ BindSig;
  
  def kw(word: String): Parser[String] = elem(TKeyword(word)) ^^ (_.asInstanceOf[TKeyword].word);
  
  def tok[T <: FToken](implicit m: Manifest[T]): Parser[T] =
    elem(m.toString(), t => m.erasure == t.getClass()) ^^ (t => t.asInstanceOf[T]);
  
  def genModuleHeader(word: String, ty: Type): Parser[Binder] = kw(word) ~> bindName ~ opt(hasType) ^^ Binder;
    
  def moduleHeader = genModuleHeader("module", Type.defaultModuleType);
  
  def sigHeader = genModuleHeader("sig", Type.defaultSigType);
  
  def moduleApp: Parser[Expr[ModPrim]] = modulePrim ~ rep(modulePrim) ^^ App[ModPrim];  
    
  def modulePrim: Parser[Expr[ModPrim]] = name ^^ ModRef ^^ Prim[ModPrim] | kw("(") ~> moduleApp <~ kw(")");
  
  def openQual: Parser[OpenQual] =
    ((kw("except") | kw("only")) ^^ (_ == "only")) ~ rep1(bindName) ^^ OpenQual;
  
  def decls: Parser[Expr[ModPrim]] = rep(decl) ^^ Module ^^ Prim[ModPrim];
  
  def sigDecls: Parser[Expr[SigPrim]] = rep(sigDecl) ^^ Sig ^^ Prim[SigPrim];
  
  def decl: Parser[Decl] =
    kw("open") ~> moduleApp ~ opt(openQual) ^^ Open |
    (kw("val") ~> (bindName ~ opt(hasType) ^^ Binder) <~ kw("is")) ~ expr(exprPrim) ^^ BindVal |
    kw("data") ~> (dataOpen ~ bindName ~ opt(parentType) <~ kw("is")) ~ ty ^^ Data |
    kw("type") ~> (bindName <~ kw("is")) ~ ty ^^ TypeAlias |
    (moduleHeader <~ kw("is")) ~ (moduleApp | decls <~ kw("end")) ^^ BindModule |
    (sigHeader <~ kw("is")) ~ sigDecls <~ kw("end") ^^ BindSig |
    kw("infix") ~> infixAssoc ~ (tok[TInt] ^^ (x => x.n)) ~ rep1(bindName) ^^ Infix;
  
  def bindName: Parser[IdDecl] = (tok[TId] ^^ (x => x.xs) | tok[TExprOp] ^^ (x => x.xs)) ^^ IdDecl;
  
  def dataOpen: Parser[Boolean] =
    kw("open") ~> success(true) |
    kw("closed") ~> success(false) |
    success(false);
    
  def parentType: Parser[Type] = kw("<:") ~> ty;

  def infixAssoc: Parser[InfixAssoc] =
    kw("left") ~> success(InfixLeft) |
    kw("right") ~> success(InfixRight) |
    success(InfixNone);
    
  def sigDecl: Parser[SigDecl] =
    kw("val") ~> bindName ~ hasType ^^ SigVal |
    kw("type") ~> bindName ~ opt(typeRel) ^^ SigType |
    kw("module") ~> bindName ~ hasType ^^ SigModule;
    
  def tyCompOp: Parser[TypeCompOp] =
    kw("<:") ~> success(OpSubType) |
    kw("<:") ~> success(OpSuperType) |
    kw(":") ~> success(OpEqualType);
    
  def typeRel: Parser[TypeRel] = tyCompOp ~ ty ^^ TypeRel;
  
  def expr(prim: Parser[Expr[ValPrim]]): Parser[Expr[ValPrim]] =
    (exprApp(prim) ^^ Some.apply) ~ rep(exprOp(prim)) ^^ OpChain[ValPrim] |
    rep1(exprOp(prim)) ^^ (xs => OpChain[ValPrim](None, xs));
  
  def exprApp(prim: Parser[Expr[ValPrim]]): Parser[Expr[ValPrim]] = prim ~ rep(prim) ^^ App[ValPrim];
  
  def exprOp(prim: Parser[Expr[ValPrim]]): Parser[(IdDecl, Expr[ValPrim])] =
    (tok[TExprOp] ^^ (x => IdDecl(x.xs))) ~ exprApp(prim) ^^ (x => (x._1, x._2));
  
  def localBind: Parser[LocalBind[ValPrim]] =
      (bindName ~ opt(hasType) <~ kw("is") ^^ Binder) ~ expr(exprPrim) ^^ LocalBind[ValPrim];
  
  def semi[T](p: Parser[T]): Parser[List[T]] = rep1sep(p, kw(";")) <~ opt(kw(";"));
  
  def localBinds: Parser[List[LocalBind[ValPrim]]] = semi(localBind);
  
  def exprEnd: Parser[List[LocalBind[ValPrim]]] =
    (opt(kw("where") ~> localBinds) ^^ (x => x.getOrElse(Nil))) <~ kw("end");
  
  def withLocalBinds(e: Expr[ValPrim], bs: List[LocalBind[ValPrim]]) = e.withLocalBinds(bs);
  
  def exprPrimDo: Parser[Expr[ValPrim]] =
    kw("fn") ~> ((rep(valParam) <~ kw("->")) ~ expr(exprPrim) ^^ Lam[ValPrim]) ~
      exprEnd ^^ withLocalBinds |
    kw("fn") ~> kw("of") ~> (semi(fnClause) ^^ LamCase ^^ Prim[ValPrim]) ~ exprEnd ^^ withLocalBinds |
    kw("case") ~> ((expr(exprPrim) <~ kw("of")) ~ semi(caseClause) ^^ Case ^^ Prim[ValPrim]) ~
      exprEnd ^^ withLocalBinds |
    kw("rec") ~> (localBinds ^^ Record ^^ Prim[ValPrim]) ~ exprEnd ^^ withLocalBinds |
    kw("begin") ~> expr(exprPrim) ~ exprEnd ^^ withLocalBinds |
    name ^^ ERef[ValPrim] |
    tok[TInt] ^^ (t => Prim[ValPrim](EInt(t.n))) |
    tok[TFloat] ^^ (t => Prim[ValPrim](EFloat(t.n))) |
    tok[TString] ^^ (t => Prim[ValPrim](EString(t.xs))) |
    tok[TChar] ^^ (t => Prim[ValPrim](EChar(t.xs))) |
    kw("?") ~> success(ToDo[ValPrim]());
  
  def exprPrim: Parser[Expr[ValPrim]] =
    exprPrimDo |
    kw("do") ~> (semi(doElem) ^^ Do ^^ Prim[ValPrim]) ~ exprEnd ^^ withLocalBinds |
    kw("let") ~> ((localBinds <~ kw("in")) ~ expr(exprPrim) ^^ Let[ValPrim]) ~ exprEnd ^^ withLocalBinds |
    kw("(") ~> expr(exprPrim) <~ kw(")");
  
  def name: Parser[IdRef] = tok[TName] ^^ (t => IdRef(t.xs)) | tok[TId] ^^ (t => IdRef(List(t.xs)));
    
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
    tok[TChar] ^^ (t => PatChar(t.xs));
  
  def patApp: Parser[Pat] = name ~ rep(pat) ^^ PatApp;
  
  def hasType: Parser[Type] = kw(":") ~> ty;
  
  def ty: Parser[Type] = tyQuants ~ tyCore ~ rep(tyConstr) ^^ Type.apply;
  
  def tyQuants: Parser[List[IdDecl]] =
    opt(kw("forall") ~> rep1(bindName) <~ kw(".")) ^^ (m => m.getOrElse(Nil));
  
  def tyCore: Parser[Expr[TyPrim]] =
    (tyApp ^^ Some.apply) ~ rep(kw("->") ~> tyApp ^^ (x => (IdDecl("->"), x))) ^^ OpChain[TyPrim];
  
  def tyApp: Parser[Expr[TyPrim]] = tyPrim ~ rep(tyPrim) ^^ App[TyPrim];
  
  def tyPrim: Parser[Expr[TyPrim]] =
    kw("(") ~> tyCore <~ kw(")") |
    kw("*") ~> success(Prim[TyPrim](TyAuto)) |
    kw("rec") ~> semi(bindName ~ (hasType ^^ Some.apply) ^^ Binder) <~ kw("end") ^^ TyRecord ^^ Prim[TyPrim] |
    name ^^ ERef[TyPrim];
  
  def tyConstr: Parser[TyConstr] = kw("with") ~> tyCore ~ tyCompOp ~ tyCore ^^ TyConstr;

}