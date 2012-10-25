package net.karlv.flang.ast

case class App[P, D](fn: Expr[P, D], args: List[Expr[P, D]]) extends Expr[P, D] {
  
}