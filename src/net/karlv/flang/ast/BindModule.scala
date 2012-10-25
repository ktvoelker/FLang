package net.karlv.flang.ast

case class BindModule(binder: Binder, body: Expr[Nothing, ModDecl]) extends Bind(binder, body) with File {
  
}