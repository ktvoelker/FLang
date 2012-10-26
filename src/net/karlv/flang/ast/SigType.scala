package net.karlv.flang.ast

case class SigType(id: Name, tyRel: Option[TyConstraint]) extends SigDecl {
  
}