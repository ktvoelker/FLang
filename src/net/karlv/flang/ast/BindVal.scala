package net.karlv.flang.ast

case class BindVal(binder: Binder, body: Expr[ValExpr, ValDecl]) extends Bind(binder, body) with ValDecl {
  
}
