package net.karlv.flang.ast

abstract class TyExpr extends UTy.Expr {
  
}

case object TyAuto extends TyExpr {
  
  override def children = Nil;
  
}

case object TyFn extends TyExpr {
  
  override def children = Nil;
  
}
