package net.karlv.flang.ast

case class Sig(decls: List[SigDecl]) extends GenModule[SigDecl](decls) {
  
}