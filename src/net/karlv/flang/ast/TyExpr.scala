package net.karlv.flang.ast

abstract class TyExpr extends Liftable[TyExpr, Nothing] {
  
  override def lift = Lift[TyExpr, Nothing](this);
  
}

case object TyAuto extends TyExpr {
  
}

case object TyFn extends TyExpr {
  
}

case class TyRecord(elems: List[Binder]) extends TyExpr {
  
}