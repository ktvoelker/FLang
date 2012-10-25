package net.karlv.flang.ast

case class Let[+P, D](binds: List[D], body: Expr[P, D]) extends Expr[P, D] {
  
}