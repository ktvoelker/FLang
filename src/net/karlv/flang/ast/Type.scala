package net.karlv.flang.ast

case class Type(quant: List[IdDecl], expr: Expr[TyPrim], constrs: List[TyConstr]) {

}

object Type {
  
  def defaultModuleType: Type = null;
  
  def defaultSigType: Type = null;

}
