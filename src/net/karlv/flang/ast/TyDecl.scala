package net.karlv.flang.ast

trait TyDecl extends UTy.Decl {

}

case class FieldDecl(id: BindName, ty: UTy.Expr) extends TyDecl {
  
}

case class Constraint(left: UTy.Expr, op: TyCompOp, right: UTy.Expr) extends TyDecl {
  
}
