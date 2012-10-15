package net.karlv.flang

import org.antlr.runtime.tree.CommonTree
import org.antlr.runtime.ANTLRStringStream
import org.antlr.runtime.CommonTokenStream

object Test {

  def main(args: Array[String]): Unit = {
    // val is = new ANTLRFileStream(args(0));
    val is = new ANTLRStringStream("module foo val x is 3");
    val lex = new FLexer(is);
    val ts = new CommonTokenStream(lex);
    val p = new FParser(ts);
    val x = p.start();
    val t = x.getTree();
    print(t.asInstanceOf[CommonTree].toStringTree());
  }

}
