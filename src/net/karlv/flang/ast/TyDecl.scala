package net.karlv.flang.ast

trait TyDecl {

}

case class BindValTy(id: Name, ty: Type) extends TyDecl {
  
}