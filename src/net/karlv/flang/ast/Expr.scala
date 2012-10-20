package net.karlv.flang.ast

abstract class Expr[P] {
  
  def withLocalBinds(bs: List[LocalBind[P]]): Expr[P] = bs match {
    case Nil => this;
    case _ => Let(bs, this);
  };

}

case class ERef[P](id: IdRef) extends Expr[P] {
  
}

case class ToDo[P]() extends Expr[P] {
  
}

case class Prim[P](val prim: P) extends Expr[P] {
  
  implicit def toPrim: P = prim;

}

case class App[P](fn: Expr[P], args: List[Expr[P]]) extends Expr[P] {
  
}

case class OpChain[P](left: Option[Expr[P]], ops: List[(IdDecl, Expr[P])]) extends Expr[P] {
  
}

case class Lam[P](params: List[Binder], body: Expr[P]) extends Expr[P] {
  
}

case class Let[P](binds: List[LocalBind[P]], body: Expr[P]) extends Expr[P] {
  
}

