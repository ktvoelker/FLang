package net.karlv.flang.ast

case class Sig(val id: IdDecl, val tyParams: List[IdDecl], val decls: List[SigDecl]) {
  
}