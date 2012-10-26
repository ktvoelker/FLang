package net.karlv.flang.ast

case class OpChain[P, D](left: Option[Expr[P, D]], ops: List[(Expr[P, D], Expr[P, D])]) extends Expr[P, D] {
  
}