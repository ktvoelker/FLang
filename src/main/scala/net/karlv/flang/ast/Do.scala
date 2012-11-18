package net.karlv.flang.ast

case class Do(elems: List[DoElem]) extends ValExpr {

  def edit(f: UVal.Expr => UVal.Expr): Do = Do(elems.map(_.editExprs(f)));
  
  def fold[T](zero: T)(f: (UVal.Expr, T) => T): T =
    elems.foldRight(zero)((elem, result) => elem.foldExprs(result)(f));

}

abstract class DoElem {

  def editExprs(f: UVal.Expr => UVal.Expr): DoElem;
  
  def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T;
  
}

case class DoBind(pat: Pat, body: UVal.Expr) extends DoElem {

  def editExprs(f: UVal.Expr => UVal.Expr): DoBind = DoBind(pat, f(body));
  
  def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T = f(body, zero);
  
}

case class DoExpr(expr: UVal.Expr) extends DoElem {

  def editExprs(f: UVal.Expr => UVal.Expr): DoExpr = DoExpr(f(expr));
  
  def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T = f(expr, zero);
  
}

case class DoLet(binds: List[UVal.Decl]) extends DoElem {

  def editExprs(f: UVal.Expr => UVal.Expr): DoLet = DoLet(binds.map(_.editExprs(f)));
  
  def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T =
    UVal.Decl.foldDeclsExprs(zero)(f)(binds);
  
}
