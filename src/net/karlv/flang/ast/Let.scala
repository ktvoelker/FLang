package net.karlv.flang.ast

case class Let[P](binds: List[LocalBind[P]], body: Expr[P]) extends Expr[P] {
  
}