package net.karlv.flang.ast

abstract class ValExpr extends UVal.Expr {

}

case class LamCase(clauses: List[LamCaseClause]) extends ValExpr {
  
  override def children = clauses.map(_.body);
  
}

case class LamCaseClause(pats: List[Pat], body: UVal.Expr) {
  
}

abstract class PrimVal extends ValExpr {
  
  override def children = Nil;
  
}

case class EInt(n: BigInt) extends PrimVal {
  
}

case class EFloat(n: BigDecimal) extends PrimVal {
  
}

case class EString(xs: String) extends PrimVal {
  
}

case class EChar(char: Char) extends PrimVal {
  
}