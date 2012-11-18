package net.karlv.flang.ast

abstract class ValExpr extends UVal.Expr {

}

case class LamCase(clauses: List[LamCaseClause]) extends ValExpr {

  override def edit(f: UVal.Expr => UVal.Expr): LamCase =
    LamCase(clauses.map(c => LamCaseClause(c.pats, f(c.body))));
  
  override def fold[T](zero: T)(f: (UVal.Expr, T) => T) =
    clauses.map(_.body).foldRight(zero)(f);
  
}

case class LamCaseClause(pats: List[Pat], body: UVal.Expr) {
  
}

abstract class PrimVal extends ValExpr {

  override def edit(f: UVal.Expr => UVal.Expr): PrimVal = this;
  
  override def fold[T](zero: T)(f: (UVal.Expr, T) => T) = zero;
  
}

case class EInt(n: BigInt) extends PrimVal {
  
}

case class EFloat(n: BigDecimal) extends PrimVal {
  
}

case class EString(xs: String) extends PrimVal {
  
}

case class EChar(char: Char) extends PrimVal {
  
}
