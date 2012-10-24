package net.karlv.flang.ast

case class Case(head: Expr[ValPrim], clauses: List[CaseClause]) extends ValPrim {
  
}

case class CaseClause(pat: Pat, body: Expr[ValPrim]) {
  
}