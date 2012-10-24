package net.karlv.flang.ast

case class SigType(id: IdDecl, tyRel: Option[TyConstraint]) extends SigDecl {
  
}