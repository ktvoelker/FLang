package net.karlv.flang.ast

abstract class ValExpr extends Liftable[ValExpr] {
  
  override def lift = Lift(this);

}

case class LamCase(clauses: List[LamCaseClause]) extends ValExpr {
  
}

case class LamCaseClause(pats: List[Pat], body: Expr[ValExpr, ValDecl]) {
  
}

case class EInt(n: BigInt) extends ValExpr {
  
}

case class EFloat(n: BigDecimal) extends ValExpr {
  
}

case class EString(xs: String) extends ValExpr {
  
}

case class EChar(char: Char) extends ValExpr {
  
}