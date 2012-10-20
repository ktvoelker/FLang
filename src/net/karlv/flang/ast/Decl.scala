package net.karlv.flang.ast

abstract class Decl {

}

case class Open(module: Expr[PrimModuleExpr], names: Option[List[IdDecl]], includeNames: Boolean) extends Decl {
  
}

object Decl {
  def parse(t: KTree): Decl = t.text match {
    case "open" => {
      val (includeNames, names) = t.second match {
        case None => (true, None);
        case Some(t) => (t.first.map(_.text) == Some("only"), Some(t.kids.drop(1).map(t => IdDecl(t.text))));
      };
      Open(Expr.parse(PrimModuleExpr.parse, t.first.get), names, includeNames)
    }
  };
}