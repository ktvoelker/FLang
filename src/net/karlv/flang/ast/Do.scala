package net.karlv.flang.ast

case class Do(elems: List[DoElem]) extends ValPrim {

}

abstract class DoElem {
  
}

case class DoLet(binds: List[LocalBind[ValPrim]]) extends DoElem {
  
}

case class DoBind(pat: Pat, body: Expr[ValPrim]) extends DoElem {
  
}

case class DoExpr(expr: Expr[ValPrim]) extends DoElem {
  
}