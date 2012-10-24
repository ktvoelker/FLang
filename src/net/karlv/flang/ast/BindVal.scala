package net.karlv.flang.ast

case class BindVal(binder: Binder, body: Expr[ValPrim]) extends Bind(binder, body) {
  
}
