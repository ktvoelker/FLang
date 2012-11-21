package net.karlv.flang.compile
import net.karlv.flang.ast._

case class SyntaxChecker[U <: Universe](implicit u: U) {

  def apply(e: u.Expr): u.Expr = e.editTree((e: u.Expr) => e match {
      case u.OpChain(left, ops) =>
        makeMembers(left.map((u.Invalid, _)).toList ++ ops) match {
          case Nil => null; // TODO throw error
          case (_, e) :: Nil => e;
          case (u.Invalid, e) :: xs => u.OpChain(Some(e), xs);
          case xs => u.OpChain(None, xs);
        };
      case e => e;
    });

  private def makeMembers(xs: List[(u.Expr, u.Expr)]): List[(u.Expr, u.Expr)] =
    null;

}
