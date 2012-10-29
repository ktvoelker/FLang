package net.karlv.flang.ast
import net.karlv.flang._
import net.karlv.flang.Util._
import scala.collection.mutable

class Universe {

  case class Env(
      val root: UMod.Record,
      val mods: mutable.Map[List[BindName], UMod.Record],
      val curMod: List[BindName],
      val locals: Map[BindName, Expr]) {

    def intoModule(name: BindName) = Env(root, mods, curMod ++ List(name), locals);

    def intoLet(moreLocals: Map[BindName, Expr]) = Env(root, mods, curMod, moreLocals ++ locals);
    
    def intoUniverse[U <: Universe](implicit u: U): U#Env = u.Env(root, mods, curMod, Map());
    
    def findModule(path: List[BindName]): Option[UMod.Record] = path match {
      case Nil => throw new Error(EImpossible);
      // enumerate prefixes of the current module path, starting with the longest
      case _ => curMod.inits.toList.reverseIterator.
    		  // see if the given relative reference exists under each prefix
    		  map(prefix => findAbsoluteModule(prefix ++ path)).
    		  // choose the first
    		  find(_.isDefined).headOption.getOrElse(None);
    }
    
    // TODO: look for the module in the table, and then fall back into the AST, evaluating nodes
    // as needed
    def findAbsoluteModule(path: List[BindName]): Option[UMod.Record] = null;

  }
  
  def emptyEnv(root: UMod.Record) = Env(root, new mutable.HashMap(), Nil, Map());

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

  case class Ref(names: List[BindName]) extends Expr {

    override def children = Nil;

    var target: Expr = null;

  }

  case object ToDo extends Expr {

    override def children = Nil;

  }

}
