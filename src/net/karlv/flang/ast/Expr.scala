package net.karlv.flang.ast

abstract class Expr[+P, +D] {
  
  def withLocalBinds[Ds >: D](bs: List[Ds]): Expr[P, Ds] = bs match {
    case Nil => this;
    case _ => Let(bs, this);
  };

}