package net.karlv.flang.ast
import net.karlv.flang._
import net.karlv.flang.Util._
import scala.collection.mutable

class Universe {

  abstract class Bind(binder: Binder, body: Expr) {

  }

  trait Expr {

    def edit(f: Expr => Expr): Expr;

    def editTree(f: Expr => Expr): Expr = f(edit(_.editTree(f)));

    def fold[T](zero: T)(f: (Expr, T) => T): T;

    def children: List[Expr] = fold[List[Expr]](Nil)(_ :: _);

    def descendants: List[Expr] = transitiveClosure[Expr](_.children)(this);

    def withLocalBinds(bs: List[Decl]): Expr = bs match {
      case Nil => this;
      case _ => Let(bs, this);
    };

  }

  trait Decl {

    def editExprs(f: Expr => Expr): Decl;

    def foldExprs[T](zero: T)(f: (Expr, T) => T): T;

    def childExprs: List[Expr] = foldExprs[List[Expr]](Nil)(_ :: _);

  }

  object Decl {

    def foldDeclsExprs[T](zero: T)(f: (Expr, T) => T)(decls: List[Decl]): T =
      decls.foldRight(zero)((decl, result) => decl.foldExprs(result)(f));

  }

  case class App(fn: Expr, args: List[Expr]) extends Expr {

    override def edit(f: Expr => Expr): App = App(f(fn), args.map(f));

    override def fold[T](zero: T)(f: (Expr, T) => T) =
      (fn :: args).foldRight(zero)(f);

  }

  case class Let(binds: List[Decl], body: Expr) extends Expr {

    override def edit(f: Expr => Expr): Let = Let(binds.map(_.editExprs(f)), f(body));

    override def fold[T](zero: T)(f: (Expr, T) => T) =
      f(body, Decl.foldDeclsExprs(zero)(f)(binds));

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

    override def edit(f: Expr => Expr): Lam = Lam(params, f(body));

    override def fold[T](zero: T)(f: (Expr, T) => T) = f(body, zero);

  }

  case class OpChain(left: Option[Expr], ops: List[(Expr, Expr)]) extends Expr {

    override def edit(f: Expr => Expr): OpChain =
      OpChain(left.map(f), ops.map(x => (f(x._1), f(x._2))));

    override def fold[T](zero: T)(f: (Expr, T) => T) =
      (left.toList ++ ops.flatMap(x => List(x._1, x._2))).foldRight(zero)(f);

  }

  case class Record(elems: List[Decl]) extends Expr {

    override def edit(f: Expr => Expr): Record = Record(elems.map(_.editExprs(f)));

    override def fold[T](zero: T)(f: (Expr, T) => T) =
      Decl.foldDeclsExprs(zero)(f)(elems);

  }

  case class Ref(name: BindName) extends Expr {

    override def edit(f: Expr => Expr): Ref = this;

    override def fold[T](zero: T)(f: (Expr, T) => T) = zero;

    var target: Expr = null;

  }
  
  case class Member(record: Expr, name: BindName) extends Expr {

    override def edit(f: Expr => Expr): Member = Member(f(record), name);
    
    override def fold[T](zero: T)(f: (Expr, T) => T) = f(record, zero);
    
  }

  case object ToDo extends Expr {

    override def edit(f: Expr => Expr): ToDo.type = this;

    override def fold[T](zero: T)(f: (Expr, T) => T) = zero;

  }

}
