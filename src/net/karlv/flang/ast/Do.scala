package net.karlv.flang.ast

case class Do(elems: List[DoElem]) extends ValExpr {

}

abstract class DoElem {
  
}

case class DoBind(pat: Pat, body: Expr[ValExpr, ValDecl]) extends DoElem {
  
}

case class DoExpr(expr: Expr[ValExpr, ValDecl]) extends DoElem {
  
}

case class DoLet(binds: List[ValDecl]) extends DoElem {
  
}
