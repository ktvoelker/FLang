package net.karlv.flang.ast

case class Module(decls: List[Decl]) extends GenModule[Decl](decls) {
  
}
