package net.karlv.flang
import java.io.FileInputStream
import net.karlv.flang.compile.Resolver
import net.karlv.flang.ast.File

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
  
  def compile(files: List[File]): Unit = {
    files.foreach(file => print(new TreeFormatter(file).str));
    Resolver.resolve(files);
  };

}
