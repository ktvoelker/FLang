package net.karlv.flang.ast

case class Lam[P, D](params: List[Binder], body: Expr[P, D]) extends Expr[P, D] {
  
}
