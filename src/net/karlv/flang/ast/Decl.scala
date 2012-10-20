package net.karlv.flang.ast

abstract class Decl {

}

case class Open(module: Expr[ModPrim], qual: Option[OpenQual]) extends Decl {
  
}

case class OpenQual(isOnly: Boolean, names: List[IdDecl]) {
  
  def isExcept = !isOnly;

}

abstract class Bind[P](id: IdDecl, ty: Type, body: Expr[P]) extends Decl {
  
}

case class BindVal(id: IdDecl, ty: Type, body: Expr[ValPrim]) extends Bind(id, ty, body) {
  
}

trait FileBind {
  
}

case class BindModule(id: IdDecl, ty: Type, body: Expr[ModPrim]) extends Bind(id, ty, body) with FileBind {
  
}

case class BindSig(id: IdDecl, ty: Type, body: Expr[SigPrim]) extends Bind(id, ty, body) with FileBind {
  
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
