package net.karlv.flang.parser

import scala.util.parsing.combinator.RegexParsers
import java.io.InputStreamReader
import java.io.InputStream
import scala.util.parsing.input.CharSequenceReader

object Lexer extends RegexParsers {

  override def skipWhitespace = false;

  def tokens: Parser[List[Token]] =
    CharSequenceReader.EofCh ~> success(List(TEndOfInput)) |
    skip ~> tokens |
    token ~ tokens ^^ mkList;
  
  def apply(is: InputStream) = parseAll(tokens, new InputStreamReader(is)) match {
    case Success(ts, _) => ts;
    case failure: NoSuccess => sys.error(failure.toString());
  }
  
  def skip: Parser[Unit] = comment | ws;
  
  def token: Parser[Token] =
    (keywords ++ List(id, name, exprOp, int, float)).
      foldRight[Parser[Token]](failure("unexpected"))(_ | _);

  def keywords: List[Parser[TKeyword]] = List(
      "type", "val", "data", "sig", "(", ")", "open", "closed", "except", "only", "is", "rec", "let", "fn",
      "case", "begin", "do", "?", "where", "end", "in", "of", "module", "with", ":", "<:", ":>", "->", "<-",
      ";", "*", ".", "forall", "infix", "left", "right").map(_ ^^ TKeyword);

  def rawId: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r;
  
  def id: Parser[TId] = rawId ^^ TId;

  def name: Parser[TName] = rawId ~ rep("""\.""".r ~> rawId) ^^ mkList ^^ TName;

  def exprOp: Parser[TExprOp] = """(\+|-|\*|/|=|<|>)+""".r ^^ TExprOp;

  def int: Parser[TInt] = """[0-9]+""".r ^^ BigInt.apply ^^ TInt;

  def float: Parser[TFloat] =
    (("""[0-9]+\.[0-9]*""" + exponent).r | ("""\.[0-9]+""" + exponent).r | ("""[0-9]+""" + exponent).r) ^^
      BigDecimal.apply ^^ TFloat;

  def exponent = """[eE][+\-]?[0-9]+""";

  // TODO better comment syntax, with nestable block comments
  def comment: Parser[Unit] = """//[^\n\r]*\r?\n""".r ~> success(())

  def ws: Parser[Unit] = """[ \t\r\n]+""".r ~> success(());

  def charContent(quote: String, card: String) =
    ("""(\\[btnfr"'\\]|\\u[0-9a-fA-F]{4}|\\[0-3]?[0-7]{1,2}|[^\\""" + quote + "])" + card).r;

  def string: Parser[TString] = "\"" ~> charContent("\"", "*") <~ "\"" ^^ TString;

  def char: Parser[TChar] = "'" ~> charContent("'", "") <~ "'" ^^ TChar;

}