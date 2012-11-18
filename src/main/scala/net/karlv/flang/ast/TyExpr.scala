package net.karlv.flang.ast

abstract class TyExpr extends UTy.Expr {
  
}

case object TyAuto extends TyExpr {
  
  override def fold[T](zero: T)(f: (UTy.Expr, T) => T) = zero;
  
}

case object TyFn extends TyExpr {
  
  override def fold[T](zero: T)(f: (UTy.Expr, T) => T) = zero;
  
}
