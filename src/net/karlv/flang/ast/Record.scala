package net.karlv.flang.ast

case class Record(elems: List[LocalBind[ValExpr]]) extends ValExpr {
  
}