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
  
  def apply(is: InputStream): ModDecl = this(Lexicon(is));

  def apply(ts: Reader[Lexicon.Token]): ModDecl = start(ts) match {
    case Success(tree, _) => tree;
    case failure: NoSuccess => sys.error(failure.toString());
  };
  
  def flip[A, B](pair: ~[A, B]): ~[B, A] = new ~(pair._2, pair._1);
  
  def start: Parser[ModDecl] = phrase(file);
  
  def file: Parser[ModDecl] = moduleHeader ~ decls ^^ BindModule | sigHeader ~ sigDecls ^^ BindSig;
  
  def kw(word: String): Parser[String] = elem(TKeyword(word)) ^^ (_.asInstanceOf[TKeyword].word);
  
  def tok[T <: FToken](implicit m: Manifest[T]): Parser[T] =
    elem(m.toString(), t => m.erasure == t.getClass()) ^^ (t => t.asInstanceOf[T]);
  
  def genModuleHeader(word: String, ty: Type): Parser[Binder] = kw(word) ~> bindName ~ opt(hasType) ^^ Binder;
    
  def moduleHeader = genModuleHeader("module", Lift(TyAuto));
  
  def sigHeader = genModuleHeader("sig", Lift(TyAuto));
  
  def moduleApp: Parser[Module] = modulePrim ~ rep(modulePrim) ^^ App[Nothing, ModDecl];  

  def modulePrim: Parser[Module] = ref | kw("(") ~> moduleApp <~ kw(")");
  
  def openQual: Parser[OpenQual] =
    ((kw("except") | kw("only")) ^^ (_ == "only")) ~ rep1(bindName) ^^ OpenQual;
  
  def decls: Parser[Module] = rep(decl) ^^ Record[ModDecl];
  
  def sigDecls: Parser[Sig] = rep(sigDecl) ^^ Record[SigDecl];
  
  def decl: Parser[ModDecl] =
    kw("open") ~> moduleApp ~ opt(openQual) ^^ Open |
    (kw("val") ~> (bindName ~ opt(hasType) ^^ Binder) <~ kw("is")) ~ expr(exprPrim) ^^ BindVal |
    kw("data") ~> (dataOpen ~ bindName ~ opt(parentType) <~ kw("is")) ~ ty ^^ Data |
    kw("type") ~> (bindName <~ kw("is")) ~ ty ^^ TypeAlias |
    (moduleHeader <~ kw("is")) ~ (moduleApp | decls <~ kw("end")) ^^ BindModule |
    (sigHeader <~ kw("is")) ~ sigDecls <~ kw("end") ^^ BindSig |
    kw("infix") ~> infixAssoc ~ (tok[TInt] ^^ (x => x.n)) ~ rep1(bindName) ^^ Infix;
  
  def bindName: Parser[BindName] = (tok[TId] ^^ (x => x.xs) | tok[TExprOp] ^^ (x => x.xs)) ^^ BindName;
  
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
    
  def tyCompOp: Parser[TyCompOp] =
    kw("<:") ~> success(OpSubTy) |
    kw("<:") ~> success(OpSuperTy) |
    kw(":") ~> success(OpEqualTy);
    
  def typeRel: Parser[TyBound] = tyCompOp ~ ty ^^ TyBound;
  
  def expr(prim: Parser[Value]): Parser[Value] =
    (exprApp(prim) ^^ Some.apply) ~ rep(exprOp(prim)) ^^ OpChain[ValExpr, ValDecl] |
    rep1(exprOp(prim)) ^^ (xs => OpChain[ValExpr, ValDecl](None, xs));
  
  def exprApp(prim: Parser[Value]): Parser[Value] =
    prim ~ rep(prim) ^^ App[ValExpr, ValDecl];
  
  def exprOp(prim: Parser[Value]): Parser[(Value, Value)] =
    (tok[TExprOp] ^^ (x => Ref(List(BindName(x.xs))))) ~ exprApp(prim) ^^ (x => (x._1, x._2));
  
  def localBind: Parser[ValDecl] =
      (bindName ~ opt(hasType) <~ kw("is") ^^ Binder) ~ expr(exprPrim) ^^ BindVal;
  
  def semi[T](p: Parser[T]): Parser[List[T]] = rep1sep(p, kw(";")) <~ opt(kw(";"));
  
  def localBinds: Parser[List[ValDecl]] = semi(localBind);
  
  def exprEnd: Parser[List[ValDecl]] =
    (opt(kw("where") ~> localBinds) ^^ (x => x.getOrElse(Nil))) <~ kw("end");
  
  def withLocalBinds(e: Value, bs: List[ValDecl]) = e.withLocalBinds(bs);
  
  def exprPrimDo: Parser[Value] =
    kw("fn") ~> ((rep(valParam) <~ kw("->")) ~ expr(exprPrim) ^^ Lam[ValExpr, ValDecl]) ~
      exprEnd ^^ withLocalBinds |
    kw("fn") ~> kw("of") ~> (semi(fnClause) ^^ LamCase ^^ Lift[ValExpr]) ~ exprEnd ^^ withLocalBinds |
    kw("case") ~> ((expr(exprPrim) <~ kw("of")) ~ semi(caseClause) ^^ Case ^^ Lift[ValExpr]) ~
      exprEnd ^^ withLocalBinds |
    kw("rec") ~> (localBinds ^^ Record[ValDecl]) ~ exprEnd ^^ withLocalBinds |
    kw("begin") ~> expr(exprPrim) ~ exprEnd ^^ withLocalBinds |
    ref |
    tok[TInt] ^^ (t => Lift[ValExpr](EInt(t.n))) |
    tok[TFloat] ^^ (t => Lift[ValExpr](EFloat(t.n))) |
    tok[TString] ^^ (t => Lift[ValExpr](EString(t.xs))) |
    tok[TChar] ^^ (t => Lift[ValExpr](EChar(t.char))) |
    kw("?") ~> success(ToDo);
  
  def exprPrim: Parser[Value] =
    exprPrimDo |
    kw("do") ~> (semi(doElem) ^^ Do ^^ Lift[ValExpr]) ~ exprEnd ^^ withLocalBinds |
    kw("let") ~> ((localBinds <~ kw("in")) ~ expr(exprPrim) ^^ Let[ValExpr, ValDecl]) ~
      exprEnd ^^ withLocalBinds |
    kw("(") ~> expr(exprPrim) <~ kw(")");
  
  def ref: Parser[Ref] = tok[TName] ^^ (t => Ref(t.xs.map(BindName))) | tok[TId] ^^ (t => Ref(List(BindName(t.xs))));

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
  
  def patApp: Parser[Pat] = ref ~ rep(pat) ^^
      (x => if (x._1.names.length == 1 && x._1.names.head.namespace == NsValues) {
        PatBind(x._1.names.head)
      } else {
        PatApp(x._1, x._2)
      });
  
  def hasType: Parser[Type] = kw(":") ~> ty;
  
  def ty: Parser[Type] = tyQuants ~ (tyCore ~ rep(tyConstr) ^^ flip ^^ Let[TyExpr, TyDecl]) ^^ Lam[TyExpr, TyDecl];
  
  def tyQuants: Parser[List[Binder]] =
    opt(kw("forall") ~> rep1(bindName ^^ {Binder(_, None)}) <~ kw(".")) ^^ (m => m.getOrElse(Nil));
  
  def tyCore: Parser[Type] =
    (tyApp ^^ Some.apply) ~ rep(kw("->") ~> tyApp ^^ (x => (Lift(TyFn), x))) ^^ OpChain[TyExpr, TyDecl];
  
  def tyApp: Parser[Type] = tyPrim ~ rep(tyPrim) ^^ App[TyExpr, TyDecl];
  
  def tyPrim: Parser[Type] =
    kw("(") ~> tyCore <~ kw(")") |
    kw("*") ~> success(Lift[TyExpr](TyAuto)) |
    kw("rec") ~> semi(bindName ~ hasType ^^ FieldDecl) <~ kw("end") ^^ Record[TyDecl] |
    ref;
  
  def tyConstr: Parser[Constraint] = kw("with") ~> tyCore ~ tyCompOp ~ tyCore ^^ Constraint;

}