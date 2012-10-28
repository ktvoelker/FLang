package net.karlv.flang.ast

case class Data(isOpen: Boolean, id: BindName, parent: Option[UTy.Expr], ty: UTy.Expr) extends ModDecl {
  
  def isClosed = !isOpen;
  
  override def childExprs = Nil;
  
}