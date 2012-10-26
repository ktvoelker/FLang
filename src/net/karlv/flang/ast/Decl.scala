package net.karlv.flang.ast

trait ModDecl {

}

trait File {
  
}

trait ValDecl {

}

abstract class Bind[P, D](binder: Binder, body: Expr[P, D]) extends ModDecl {
  
}

case class BindModule(binder: Binder, body: Expr[Nothing, ModDecl]) extends Bind(binder, body) with File {
  
}

case class BindSig(binder: Binder, body: Expr[Nothing, SigDecl]) extends Bind(binder, body) with File {
  
}

case class BindVal(binder: Binder, body: Expr[ValExpr, ValDecl]) extends Bind(binder, body) with ValDecl {
  
}

case class TypeAlias(id: Name, ty: Type) extends ModDecl {
  
}
