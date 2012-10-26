package net.karlv.flang.ast

abstract class TyExpr extends Liftable[TyExpr] {
  
  override def lift = Lift(this);
  
}

case object TyAuto extends TyExpr {
  
}

case object TyFn extends TyExpr {
  
}
