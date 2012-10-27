package net.karlv.flang.ast

abstract class Expr[+P, +D] {
  
  def withLocalBinds[Ds >: D](bs: List[Ds]): Expr[P, Ds] = bs match {
    case Nil => this;
    case _ => Let(bs, this);
  };

}

case class App[P, D](fn: Expr[P, D], args: List[Expr[P, D]]) extends Expr[P, D] {
  
}

case class Let[+P, D](binds: List[D], body: Expr[P, D]) extends Expr[P, D] {
  
}

case class Lam[P, D](params: List[Binder], body: Expr[P, D]) extends Expr[P, D] {
  
}

case class Lift[P](val expr: P) extends Expr[P, Nothing] {
  
  implicit def unlift: P = expr;

}

abstract trait Liftable[P] {
  
  implicit def lift: Lift[P];

}

case class OpChain[P, D](left: Option[Expr[P, D]], ops: List[(Expr[P, D], Expr[P, D])]) extends Expr[P, D] {
  
}

case class Record[D](elems: List[D]) extends Expr[Nothing, D] {
  
}

case class Ref(names: List[BindName]) extends Expr[Nothing, Nothing] {
  
}

case object ToDo extends Expr[Nothing, Nothing] {
  
}
