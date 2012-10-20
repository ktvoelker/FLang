package net.karlv.flang.ast

case class Type(quant: List[IdDecl], expr: Expr[TyPrim], constrs: List[TyConstr]) {

}

object Type {
  
  def defaultModuleType: Type = null;
  
  def defaultSigType: Type = null;

}

case class TyConstr(left: Expr[TyPrim], op: TypeCompOp, right: Expr[TyPrim]) {
  
}

abstract class TyPrim {
  
}

case object TyAuto extends TyPrim {
  
}

case object TyFn extends TyPrim {
  
}

case class TyRecord(elems: List[Binder]) extends TyPrim {
  
}
