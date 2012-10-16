package net.karlv.flang

import scala.collection.JavaConversions._
import org.antlr.runtime.tree.CommonTree
import org.antlr.runtime.ANTLRStringStream
import org.antlr.runtime.CommonTokenStream
import org.antlr.runtime.ANTLRFileStream

object Main {

  def main(args: Array[String]): Unit = {
    val is = new ANTLRFileStream(args(0));
    val lex = new FLexer(is);
    val ts = new CommonTokenStream(lex);
    val p = new FParser(ts);
    val x = p.start();
    val t = x.getTree();
    dumpTree(t, "");
  }

  def dumpTree(t: AnyRef, indent: String): Unit = {
    if (t == null) {
      println("Null");
      return;
    }
    t match {
      case t: CommonTree => {
        print(indent);
        println(t.getText());
        val indentMore = indent + "..";
        val cs = t.getChildren();
        if (cs == null) {
          return;
        }
        cs.toList.asInstanceOf[List[AnyRef]].foreach(t => {
          dumpTree(t, indentMore);
        });
      }
      case _ => {
        println("Unexpected: " + t.toString());
      }
    }
  }

}
