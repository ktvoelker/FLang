package net.karlv.flang
import java.io.FileInputStream
import net.karlv.flang.ast._
import net.karlv.flang.compile._

object Main {
  
  def help(_ignore: FileInputStream): Unit = {
    println("Usage: java -jar FLang.jar FILE ...");
    System.exit(1);
  };

  def main(args: Array[String]): Unit = {
    val streams = args.toList.map(new FileInputStream(_));
    compile(streams.map(parser.Syntax(_)));
    streams.foreach(_.close);
  };
  
  def compile(files: List[ModDecl]): Unit = {
    val root = UMod.Record(files);
    TypeChecker(ModInterpreter(new Env(root), root));
    files.foreach(file => print(new TreeFormatter(file).str));
  };

}
