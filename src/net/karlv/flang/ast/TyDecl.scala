package net.karlv.flang.ast

trait TyDecl {

}

case class FieldDecl(id: BindName, ty: Type) extends TyDecl {
  
}

case class Constraint(left: Expr[TyExpr, TyDecl], op: TyCompOp, right: Expr[TyExpr, TyDecl]) extends TyDecl {
  
}
