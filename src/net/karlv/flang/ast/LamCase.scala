package net.karlv.flang.ast

case class LamCase(clauses: List[LamCaseClause]) extends ValExpr {
  
}

case class LamCaseClause(pats: List[Pat], body: Expr[ValExpr, ValDecl]) {
  
}