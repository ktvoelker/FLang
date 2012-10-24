package net.karlv.flang.ast

case class BindVal(binder: Binder, body: Expr[ValExpr]) extends Bind(binder, body) {
  
}
