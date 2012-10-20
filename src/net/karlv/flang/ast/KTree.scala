package net.karlv.flang.ast

import scala.collection.JavaConversions._
import org.antlr.runtime.tree.CommonTree

class KTree(val orig: CommonTree) {

  def kids: List[KTree] = {
    val cs = orig.getChildren();
    return if (cs == null) (List()) else (cs.toList.asInstanceOf[List[CommonTree]].map(fromCommonTree));
  }
  
  def first: Option[KTree] = kids.headOption;
  
  def second: Option[KTree] = kids.drop(1).headOption;
  
  def last: Option[KTree] = kids.lastOption;
  
  def is(xs: String): Boolean = xs == text;
  
  def text: String = orig.getText();
  
  def marked(label: String): List[KTree] = for (
    i <- kids.filter(_.is(label));
    k <- i.kids
  ) yield k;
  
  def oneMarked(label: String): Option[KTree] = marked(label).headOption;
  
  def id: Option[IdDecl] = oneMarked("IDENT").map(t => IdDecl(t.text));
  
  def ty: Option[Type] = oneMarked(":").map(Type.parse);
  
  def tyParams: List[IdDecl] = marked("TYPE_PARAMS").map(t => IdDecl(t.text));
  
  def decls: List[Decl] = marked("DECLS").map(Decl.parse);

}
