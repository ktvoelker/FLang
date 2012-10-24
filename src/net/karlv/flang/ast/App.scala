package net.karlv.flang.ast

case class App[P](fn: Expr[P], args: List[Expr[P]]) extends Expr[P] {
  
}