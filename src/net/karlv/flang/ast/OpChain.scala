package net.karlv.flang.ast

case class OpChain[P, D](left: Option[Expr[P, D]], ops: List[(IdDecl, Expr[P, D])]) extends Expr[P, D] {
  
}