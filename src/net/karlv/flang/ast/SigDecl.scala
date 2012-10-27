package net.karlv.flang.ast

trait SigDecl {

}

case class SigModule(id: BindName, ty: Type) extends SigDecl {
  
}

case class SigType(id: BindName, tyRel: Option[TyBound]) extends SigDecl {
  
}

case class TyBound(compOp: TyCompOp, right: Type) {
  
}

case class SigVal(id: BindName, ty: Type) extends SigDecl {
  
}
