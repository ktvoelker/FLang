package net.karlv.flang.ast

case class Open(module: UMod.Expr, qual: Option[OpenQual]) extends ModDecl {

  override def editExprs(f: UMod.Expr => UMod.Expr): Open = Open(f(module), qual);
  
  override def foldExprs[T](zero: T)(f: (UMod.Expr, T) => T): T = f(module, zero);
  
}

case class OpenQual(isOnly: Boolean, names: List[BindName]) {
  
  def isExcept = !isOnly;

}
