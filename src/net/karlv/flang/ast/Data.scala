package net.karlv.flang.ast

case class Data(isOpen: Boolean, id: IdDecl, parent: Option[Type], ty: Type) extends ModDecl {
  
  def isClosed = !isOpen;
  
}