package net.karlv.flang.ast

abstract class ValPrim {
  
  implicit def toExpr: Expr[ValPrim] = Prim(this);

}

case class EInt(n: BigInt) extends ValPrim {
  
}

case class EFloat(n: BigDecimal) extends ValPrim {
  
}

case class EString(xs: String) extends ValPrim {
  
}

case class EChar(char: Char) extends ValPrim {
  
}