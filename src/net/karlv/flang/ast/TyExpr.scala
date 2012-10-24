package net.karlv.flang.ast

abstract class TyExpr {
  
}

case object TyAuto extends TyExpr {
  
}

case object TyFn extends TyExpr {
  
}

case class TyRecord(elems: List[Binder]) extends TyExpr {
  
}