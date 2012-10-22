package net.karlv.flang.ast

abstract class Decl {

}

case class Open(module: Expr[ModPrim], qual: Option[OpenQual]) extends Decl {
  
}

case class OpenQual(isOnly: Boolean, names: List[IdDecl]) {
  
  def isExcept = !isOnly;

}

abstract class Bind[P](binder: Binder, body: Expr[P]) extends Decl {
  
}

case class BindVal(binder: Binder, body: Expr[ValPrim]) extends Bind(binder, body) {
  
}

trait FileBind {
  
}

case class BindModule(binder: Binder, body: Expr[ModPrim]) extends Bind(binder, body) with FileBind {
  
}

case class BindSig(binder: Binder, body: Expr[SigPrim]) extends Bind(binder, body) with FileBind {
  
}

case class Infix(assoc: InfixAssoc, prec: BigInt, ids: List[IdDecl]) extends Decl {
  
}

abstract class InfixAssoc {}
case object InfixLeft extends InfixAssoc {}
case object InfixRight extends InfixAssoc {}
case object InfixNone extends InfixAssoc {}

case class Data(isOpen: Boolean, id: IdDecl, parent: Option[Type], ty: Type) extends Decl {
  
  def isClosed = !isOpen;
  
}

case class TypeAlias(id: IdDecl, ty: Type) extends Decl {
  
}
