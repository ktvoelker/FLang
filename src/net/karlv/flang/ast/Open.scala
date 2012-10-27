package net.karlv.flang.ast

case class Open(module: Expr[Nothing, ModDecl], qual: Option[OpenQual]) extends ModDecl {
  
}

case class OpenQual(isOnly: Boolean, names: List[BindName]) {
  
  def isExcept = !isOnly;

}
