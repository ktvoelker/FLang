package net.karlv.flang.ast

trait TyDecl extends UTy.Decl {

}

case class FieldDecl(id: BindName, ty: UTy.Expr) extends TyDecl {
  
  override def foldExprs[T](zero: T)(f: (UTy.Expr, T) => T): T = f(ty, zero);
  
}

case class Constraint(left: UTy.Expr, op: TyCompOp, right: UTy.Expr) extends TyDecl {
  
  override def foldExprs[T](zero: T)(f: (UTy.Expr, T) => T): T =
    f(left, f(right, zero));
  
}
