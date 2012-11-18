package net.karlv.flang.ast

case class Case(head: UVal.Expr, clauses: List[CaseClause]) extends ValExpr {
  
  override def fold[T](zero: T)(f: (UVal.Expr, T) => T): T =
    (head :: clauses.map(_.body)).foldRight(zero)(f);
  
}

case class CaseClause(pat: Pat, body: UVal.Expr) {
  
}
