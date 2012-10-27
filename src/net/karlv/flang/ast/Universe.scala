package net.karlv.flang.ast

class Universe {

  abstract class Bind(binder: Binder, body: Expr) {
    
  }

  trait Expr {
    
    def withLocalBinds(bs: List[Decl]): Expr = bs match {
      case Nil => this;
      case _ => Let(bs, this);
    };
  
  }
  
  trait Decl {
    
  }
  
  case class App(fn: Expr, args: List[Expr]) extends Expr {
    
  }
  
  case class Let(binds: List[Decl], body: Expr) extends Expr {
    
  }
  
  case class Lam(params: List[Binder], body: Expr) extends Expr {
    
  }
  
  case class OpChain(left: Option[Expr], ops: List[(Expr, Expr)]) extends Expr {
    
  }
  
  case class Record(elems: List[Decl]) extends Expr {
    
  }
  
  case class Ref(names: List[BindName]) extends Expr {
    
  }
  
  case object ToDo extends Expr {
    
  }

}
