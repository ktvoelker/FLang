package net.karlv.flang.ast

case class Module(val id: IdDecl, val tyParams: List[IdDecl], val ty: Type, val decls: List[Decl]) {

}

object Module {

  def parse(t: KTree): Module =
    Module(IdDecl(t.first.get.text), t.tyParams, t.ty.getOrElse(Type.defaultModuleType), t.decls);

}

abstract class PrimModuleExpr {
  
}

object PrimModuleExpr {
  
  def parse(t: KTree): PrimModuleExpr = null;

}