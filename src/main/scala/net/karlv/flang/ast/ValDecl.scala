package net.karlv.flang.ast

trait ValDecl extends UVal.Decl {
  
}

case class BindLocalVal(binder: Binder, body: UVal.Expr) extends UVal.Bind(binder, body) with ValDecl {
  
  override def foldExprs[T](zero: T)(f: (UVal.Expr, T) => T): T = f(body, zero);
  
}
