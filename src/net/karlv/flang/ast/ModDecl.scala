package net.karlv.flang.ast

trait ModDecl extends UMod.Decl {

}

case class BindModule(binder: Binder, body: UMod.Expr) extends UMod.Bind(binder, body) with ModDecl {
 
  override def childExprs = List(body);
  
}

case class BindSig(binder: Binder, body: USig.Expr) extends USig.Bind(binder, body) with ModDecl {
  
  override def childExprs = Nil;
  
}

case class BindType(binder: Binder, body: UTy.Expr) extends UTy.Bind(binder, body) with ModDecl {
  
  override def childExprs = Nil;
  
}

case class BindVal(binder: Binder, body: UVal.Expr) extends UVal.Bind(binder, body) with ModDecl {
  
  override def childExprs = Nil;
  
}
