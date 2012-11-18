package net.karlv.flang.ast

trait TyDecl extends UTy.Decl {

}

case class FieldDecl(id: BindName, ty: UTy.Expr) extends TyDecl {

  override def editExprs(f: UTy.Expr => UTy.Expr): FieldDecl = FieldDecl(id, f(ty));
  
  override def foldExprs[T](zero: T)(f: (UTy.Expr, T) => T): T = f(ty, zero);
  
}

case class Constraint(left: UTy.Expr, op: TyCompOp, right: UTy.Expr) extends TyDecl {

  override def editExprs(f: UTy.Expr => UTy.Expr): Constraint =
    Constraint(f(left), op, f(right));
  
  override def foldExprs[T](zero: T)(f: (UTy.Expr, T) => T): T =
    f(left, f(right, zero));
  
}
