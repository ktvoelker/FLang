package net.karlv.flang.ast

abstract class TyExpr extends Liftable[TyExpr, TyDecl] {
  
  override def lift = Lift[TyExpr, TyDecl](this);
  
}

case object TyAuto extends TyExpr {
  
}

case object TyFn extends TyExpr {
  
}
