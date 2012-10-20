package net.karlv.flang.ast

abstract class SigDecl {

}

case class SigVal(id: IdDecl, ty: Type) extends SigDecl {
  
}

case class SigType(id: IdDecl, tyRel: Option[TypeRel]) extends SigDecl {
  
}

case class TypeRel(compOp: TypeCompOp, ty: Type) {
  
}

case class SigModule(id: IdDecl, ty: Type) extends SigDecl {
  
}
