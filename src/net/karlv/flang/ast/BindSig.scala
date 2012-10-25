package net.karlv.flang.ast

case class BindSig(binder: Binder, body: Expr[Nothing, SigDecl]) extends Bind(binder, body) with File {
  
}