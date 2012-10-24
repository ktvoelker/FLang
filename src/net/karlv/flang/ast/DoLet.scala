package net.karlv.flang.ast

case class DoLet(binds: List[LocalBind[ValExpr]]) extends DoElem {
  
}