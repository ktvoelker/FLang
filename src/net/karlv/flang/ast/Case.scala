package net.karlv.flang.ast

case class Case(head: UVal.Expr, clauses: List[CaseClause]) extends ValExpr {
  
}

case class CaseClause(pat: Pat, body: UVal.Expr) {
  
}