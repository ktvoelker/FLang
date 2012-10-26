package net.karlv.flang.ast

trait SigDecl {

}

case class SigModule(id: Name, ty: Type) extends SigDecl {
  
}

case class SigType(id: Name, tyRel: Option[TyBound]) extends SigDecl {
  
}

case class TyBound(compOp: TyCompOp, right: Type) {
  
}

case class SigVal(id: Name, ty: Type) extends SigDecl {
  
}
