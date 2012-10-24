package net.karlv.flang.ast

case class BindSig(binder: Binder, body: Expr[SigPrim]) extends Bind(binder, body) with FileBind {
  
}