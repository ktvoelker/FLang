package net.karlv.flang
import java.io.FileInputStream

object Main {

  def main(args: Array[String]): Unit = {
    val is = new FileInputStream(args(0));
    val ast = parser.Syntax(is);
    print(new TreeFormatter(ast).str);
  }

}
