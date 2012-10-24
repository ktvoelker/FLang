package net.karlv.flang.ast

abstract class TyPrim {
  
}

case object TyAuto extends TyPrim {
  
}

case object TyFn extends TyPrim {
  
}

case class TyRecord(elems: List[Binder]) extends TyPrim {
  
}