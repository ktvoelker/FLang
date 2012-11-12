package net.karlv.flang.compile
import net.karlv.flang._
import net.karlv.flang.ast._
import scala.collection.mutable

case class Env[U <: Universe](
  val root: UMod.Record,
  val mods: mutable.Map[List[BindName], UMod.Record],
  val curMod: List[BindName],
  val locals: Map[BindName, U#Expr]) {

  def this(root: UMod.Record) = this(root, new mutable.HashMap(), Nil, Map());

  def intoModule(name: BindName) = Env(root, mods, curMod ++ List(name), locals);

  def intoLet(moreLocals: Map[BindName, U#Expr]) = Env(root, mods, curMod, moreLocals ++ locals);

  def intoUniverse[V <: Universe]: Env[V] = Env[V](root, mods, curMod, Map());

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
