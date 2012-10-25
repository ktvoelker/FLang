package net.karlv.flang.ast

case class Case(head: Expr[ValExpr, ValDecl], clauses: List[CaseClause]) extends ValExpr {
  
}

case class CaseClause(pat: Pat, body: Expr[ValExpr, ValDecl]) {
  
}