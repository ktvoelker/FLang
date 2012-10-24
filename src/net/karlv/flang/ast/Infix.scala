package net.karlv.flang.ast

case class Infix(assoc: InfixAssoc, prec: BigInt, ids: List[IdDecl]) extends Decl {
  
}

abstract class InfixAssoc {}
case object InfixLeft extends InfixAssoc {}
case object InfixRight extends InfixAssoc {}
case object InfixNone extends InfixAssoc {}
