package net.karlv.flang.ast

case class OpChain[P](left: Option[Expr[P]], ops: List[(IdDecl, Expr[P])]) extends Expr[P] {
  
}