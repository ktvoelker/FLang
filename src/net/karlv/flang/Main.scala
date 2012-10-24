package net.karlv.flang
import java.io.FileInputStream

object Main {
  
  def lex(is: FileInputStream): Unit = {
    var r = parser.Lexicon(is);
    while (!r.atEnd) {
      println(r.first);
      r = r.rest;
    }
  };
  
  def parse(is: FileInputStream): Unit = {
    val ast = parser.Syntax(is);
    print(new TreeFormatter(ast).str);
  };

  def main(args: Array[String]): Unit = {
    if (args.length == 2) {
      val is = new FileInputStream(args(1));
      args(0) match {
        case "lex" => {
          lex(is);
          return;
        };
        case "parse" => {
          parse(is);
          return;
        };
      };
    }
    println("Usage: java -jar FLang.jar (lex|parse) FILE");
  }

}
