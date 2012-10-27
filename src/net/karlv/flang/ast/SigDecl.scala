package net.karlv.flang.ast

trait SigDecl extends USig.Decl {

}

case class SigModule(id: BindName, ty: UTy.Expr) extends SigDecl {
  
}

case class SigType(id: BindName, tyRel: Option[TyBound]) extends SigDecl {
  
}

case class TyBound(compOp: TyCompOp, right: UTy.Expr) {
  
}

case class SigVal(id: BindName, ty: UTy.Expr) extends SigDecl {
  
}
