package net.karlv.flang.ast

abstract class ValExpr extends Liftable[ValExpr, ValDecl] {
  
  override def lift = Lift(this);

}

case class EInt(n: BigInt) extends ValExpr {
  
}

case class EFloat(n: BigDecimal) extends ValExpr {
  
}

case class EString(xs: String) extends ValExpr {
  
}

case class EChar(char: Char) extends ValExpr {
  
}