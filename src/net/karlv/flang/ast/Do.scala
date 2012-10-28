package net.karlv.flang.ast

case class Do(elems: List[DoElem]) extends ValExpr {
  
  override def children = elems.flatMap(_.childExprs);

}

abstract class DoElem {
  
  def childExprs: List[UVal.Expr];
  
}

case class DoBind(pat: Pat, body: UVal.Expr) extends DoElem {
  
  override def childExprs = List(body);
  
}

case class DoExpr(expr: UVal.Expr) extends DoElem {
  
  override def childExprs = List(expr);
  
}

case class DoLet(binds: List[ValDecl]) extends DoElem {
  
  override def childExprs = binds.flatMap(_.childExprs);
  
}
