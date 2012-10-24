package net.karlv.flang.ast

case class Prim[P](val prim: P) extends Expr[P] {
  
  implicit def toPrim: P = prim;

}