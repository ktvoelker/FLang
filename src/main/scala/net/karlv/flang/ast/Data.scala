package net.karlv.flang.ast

case class Data(isOpen: Boolean, id: BindName, parent: Option[UTy.Expr], ty: UTy.Expr) extends ModDecl {
  
  def isClosed = !isOpen;

  override def editExprs(f: UMod.Expr => UMod.Expr): Data = this;
  
  override def foldExprs[T](zero: T)(f: (UMod.Expr, T) => T): T = zero;
  
}
