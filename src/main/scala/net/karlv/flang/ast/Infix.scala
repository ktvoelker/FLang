package net.karlv.flang.ast

case class Infix(assoc: InfixAssoc, prec: BigInt, ids: List[BindName]) extends ModDecl {
  
  override def foldExprs[T](zero: T)(f: (UMod.Expr, T) => T): T = zero;
  
}

abstract class InfixAssoc {}
case object InfixLeft extends InfixAssoc {}
case object InfixRight extends InfixAssoc {}
case object InfixNone extends InfixAssoc {}
