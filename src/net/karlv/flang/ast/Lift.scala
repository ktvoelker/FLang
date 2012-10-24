package net.karlv.flang.ast

case class Lift[P](val expr: P) extends Expr[P] {
  
  implicit def lift: P = expr;

}