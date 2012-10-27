package net.karlv.flang.ast

case class Open(module: UMod.Expr, qual: Option[OpenQual]) extends ModDecl {
  
}

case class OpenQual(isOnly: Boolean, names: List[BindName]) {
  
  def isExcept = !isOnly;

}
