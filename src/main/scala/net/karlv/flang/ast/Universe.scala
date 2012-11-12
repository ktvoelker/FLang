package net.karlv.flang.ast
import net.karlv.flang._
import net.karlv.flang.Util._
import scala.collection.mutable

class Universe {

  abstract class Bind(binder: Binder, body: Expr) {

  }

  trait Expr {

    def children: List[Expr];

    def descendants: List[Expr] = transitiveClosure[Expr](_.children)(this);

    def withLocalBinds(bs: List[Decl]): Expr = bs match {
      case Nil => this;
      case _ => Let(bs, this);
    };

  }

  trait Decl {

    def childExprs: List[Expr];

  }

  case class App(fn: Expr, args: List[Expr]) extends Expr {

    override def children = fn :: args;

  }

  case class Let(binds: List[Decl], body: Expr) extends Expr {

    override def children = body :: binds.flatMap(_.childExprs);

    var parentLet: Option[Let] = None;

    {
      def f(expr: Expr): Unit = expr match {
        case let: Let => let.parentLet = Some(this);
        case _ => expr.children.foreach(f);
      };
      children.foreach(f);
    }

  }

  case class Lam(params: List[Binder], body: Expr) extends Expr {

    override def children = List(body);

  }

  case class OpChain(left: Option[Expr], ops: List[(Expr, Expr)]) extends Expr {

    override def children = left.toList ++ ops.flatMap(x => List(x._1, x._2));

  }

  case class Record(elems: List[Decl]) extends Expr {

    override def children = elems.flatMap(_.childExprs);

  }

  case class Ref(name: BindName) extends Expr {

    override def children = Nil;

    var target: Expr = null;

  }
  
  case class Member(record: Expr, name: BindName) extends Expr {
    
    override def children = List(record);
    
  }

  case object ToDo extends Expr {

    override def children = Nil;

  }

}
