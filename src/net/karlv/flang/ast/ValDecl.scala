package net.karlv.flang.ast

trait ValDecl extends UVal.Decl {
  
}

case class BindLocalVal(binder: Binder, body: UVal.Expr) extends UVal.Bind(binder, body) with ValDecl {
  
}
