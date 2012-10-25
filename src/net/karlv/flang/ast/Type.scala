package net.karlv.flang.ast

case class Type(quant: List[IdDecl], expr: Expr[TyExpr, ModDecl], constrs: List[TyRel]) {

}

object Type {
  
  def defaultModuleType: Type = null;
  
  def defaultSigType: Type = null;

}
