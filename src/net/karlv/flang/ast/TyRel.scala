package net.karlv.flang.ast

case class TyRel(left: Expr[TyExpr, ModDecl], op: TypeCompOp, right: Expr[TyExpr, ModDecl]) {
  
}

case class TyConstraint(compOp: TypeCompOp, right: Type) {
  
}

abstract class TypeCompOp {}
case object OpSubType extends TypeCompOp {}
case object OpSuperType extends TypeCompOp {}
case object OpEqualType extends TypeCompOp {}
