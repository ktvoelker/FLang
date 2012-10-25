package net.karlv.flang.ast

case class Lift[P, D](val expr: P) extends Expr[P, D] {
  
  implicit def unlift: P = expr;

}

abstract trait Liftable[P, D] {
  
  implicit def lift: Lift[P, D];

}
