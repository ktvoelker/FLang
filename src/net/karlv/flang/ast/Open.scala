package net.karlv.flang.ast

case class Open(module: Expr[ModPrim], qual: Option[OpenQual]) extends Decl {
  
}

case class OpenQual(isOnly: Boolean, names: List[IdDecl]) {
  
  def isExcept = !isOnly;

}
