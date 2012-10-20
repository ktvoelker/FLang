package net.karlv.flang.ast

case class IdDecl(val name: String) {
  
}

case class IdRef(val names: List[String]) {
  
}

case class Binder(id: IdDecl, ty: Option[Type]) {
  
}

case class LocalBind[P](binder: Binder, body: Expr[P]) {
  
}

abstract class TypeCompOp {}
case object OpSubType extends TypeCompOp {}
case object OpSuperType extends TypeCompOp {}
case object OpEqualType extends TypeCompOp {}
