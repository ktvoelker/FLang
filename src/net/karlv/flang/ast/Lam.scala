package net.karlv.flang.ast

case class Lam[P](params: List[Binder], body: Expr[P]) extends Expr[P] {
  
}
