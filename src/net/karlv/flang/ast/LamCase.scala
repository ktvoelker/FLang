package net.karlv.flang.ast

case class LamCase(clauses: List[LamCaseClause]) extends ValPrim {
  
}

case class LamCaseClause(pats: List[Pat], body: Expr[ValPrim]) {
  
}