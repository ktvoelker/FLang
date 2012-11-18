package net.karlv.flang.ast

trait ModDecl extends UMod.Decl {

}

case class BindModule(binder: Binder, body: UMod.Expr)
extends UMod.Bind(binder, body) with ModDecl {

  override def editExprs(f: UMod.Expr => UMod.Expr): BindModule =
    BindModule(binder, f(body));
 
  override def foldExprs[T](zero: T)(f: (UMod.Expr, T) => T): T = f(body, zero);
  
}

case class BindSig(binder: Binder, body: USig.Expr)
extends USig.Bind(binder, body) with ModDecl {

  override def editExprs(f: UMod.Expr => UMod.Expr): BindSig = this;
  
  override def foldExprs[T](zero: T)(f: (UMod.Expr, T) => T): T = zero;
  
}

case class BindType(binder: Binder, body: UTy.Expr)
extends UTy.Bind(binder, body) with ModDecl {

  override def editExprs(f: UMod.Expr => UMod.Expr): BindType = this;
  
  override def foldExprs[T](zero: T)(f: (UMod.Expr, T) => T): T = zero;
  
}

case class BindVal(binder: Binder, body: UVal.Expr)
extends UVal.Bind(binder, body) with ModDecl {

  override def editExprs(f: UMod.Expr => UMod.Expr): BindVal = this;
  
  override def foldExprs[T](zero: T)(f: (UMod.Expr, T) => T): T = zero;
  
}
