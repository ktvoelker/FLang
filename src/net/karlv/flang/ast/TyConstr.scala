package net.karlv.flang.ast

case class TyConstr(left: Expr[TyPrim], op: TypeCompOp, right: Expr[TyPrim]) {
  
}

abstract class TypeCompOp {}
case object OpSubType extends TypeCompOp {}
case object OpSuperType extends TypeCompOp {}
case object OpEqualType extends TypeCompOp {}
