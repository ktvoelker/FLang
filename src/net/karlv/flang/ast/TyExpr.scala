package net.karlv.flang.ast

abstract class TyExpr extends UTy.Expr {
  
}

case object TyAuto extends TyExpr {
  
}

case object TyFn extends TyExpr {
  
}
