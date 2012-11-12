package net.karlv.flang.ast

case class Case(head: UVal.Expr, clauses: List[CaseClause]) extends ValExpr {
  
  override def children = head :: clauses.map(_.body);
  
}

case class CaseClause(pat: Pat, body: UVal.Expr) {
  
}