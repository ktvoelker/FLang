package net.karlv.flang.ast

case class Do(elems: List[DoElem]) extends ValExpr {
  
  def fold[T](zero: T)(f: (UVal.Expr, T) => T): T =
    elems.foldRight(zero)((elem, result) => elem.foldExprs(result)(f));

}

abstract class DoElem {
  
  def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T;
  
}

case class DoBind(pat: Pat, body: UVal.Expr) extends DoElem {
  
  def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T = f(body, zero);
  
}

case class DoExpr(expr: UVal.Expr) extends DoElem {
  
  def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T = f(expr, zero);
  
}

case class DoLet(binds: List[ValDecl]) extends DoElem {
  
  def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T =
    UVal.Decl.foldDeclsExprs(zero)(f)(binds);
  
}
