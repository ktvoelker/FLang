package net.karlv.flang.ast

abstract class ValPrim {
  
  implicit def toExpr: Expr[ValPrim] = Prim(this);

}

case class LamCase(clauses: List[LamCaseClause]) extends ValPrim {
  
}

case class LamCaseClause(pats: List[Pat], body: Expr[ValPrim]) {
  
}

case class Case(head: Expr[ValPrim], clauses: List[CaseClause]) extends ValPrim {
  
}

case class CaseClause(pat: Pat, body: Expr[ValPrim]) {
  
}

case class Record(elems: List[LocalBind[ValPrim]]) extends ValPrim {
  
}

case class EInt(n: BigInt) extends ValPrim {
  
}

case class EFloat(n: BigDecimal) extends ValPrim {
  
}

case class EString(xs: String) extends ValPrim {
  
}

case class EChar(char: Char) extends ValPrim {
  
}