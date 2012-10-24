package net.karlv.flang.ast

case class BindModule(binder: Binder, body: Expr[ModPrim]) extends Bind(binder, body) with FileBind {
  
}