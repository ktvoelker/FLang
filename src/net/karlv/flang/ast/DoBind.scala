package net.karlv.flang.ast

case class DoBind(pat: Pat, body: Expr[ValExpr, ValDecl]) extends DoElem {
  
}