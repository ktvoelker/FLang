package net.karlv.flang.ast

case class DoBind(pat: Pat, body: Expr[ValPrim]) extends DoElem {
  
}