package net.karlv.flang.ast

case class Record[D](elems: List[D]) extends Expr[Nothing, D] {
  
}