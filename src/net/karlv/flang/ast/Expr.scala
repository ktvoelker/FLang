package net.karlv.flang.ast

abstract class Expr[Prim] {

}

object Expr {
  def parse[Prim](p: KTree => Prim, t: KTree): Expr[Prim] = null;
}