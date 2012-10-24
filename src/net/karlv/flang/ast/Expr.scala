package net.karlv.flang.ast

abstract class Expr[P] {
  
  def withLocalBinds(bs: List[LocalBind[P]]): Expr[P] = bs match {
    case Nil => this;
    case _ => Let(bs, this);
  };

}