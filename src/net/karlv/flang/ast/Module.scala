package net.karlv.flang.ast

abstract class GenModPrim[+D] {
  
}

case class ModRef(id: IdRef) extends GenModPrim[Nothing] {
  
}

abstract class GenModule[D](decls: List[D]) extends GenModPrim[D] {
  
}

case class Module(decls: List[Decl]) extends GenModule[Decl](decls) {
  
}

case class Sig(decls: List[SigDecl]) extends GenModule[SigDecl](decls) {
  
}