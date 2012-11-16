package net.karlv.flang.parser

import scala.util.parsing.combinator.RegexParsers
import java.io.InputStreamReader
import java.io.InputStream
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.input.Reader
import scala.collection.immutable.PagedSeq

object Lexicon extends Lexical with RegexParsers {
  
  override type Elem = Char;
  
  def apply(is: InputStream): Reader[Token] =
    new Scanner(new PagedSeqReader(new PagedSeq((a, b, c) => new InputStreamReader(is).read(a, b, c))));
  
  override def whitespace: Parser[Unit] = comment | ws;
  
  override def token: Parser[Token] =
    foldLeft1[Parser[Token]](_ | _)(
      keywords ++ List(id, exprOp, int, float, string, char));
  
  def foldLeft1[A](op: (A, A) => A)(xs: Iterable[A]) = xs.tail.foldLeft(xs.head)(op);

  def keywords: List[Parser[TKeyword]] = List(
      "type", "val", "data", "sig", "(", ")", "open", "closed", "except", "only", "is",
      "rec", "let", "fn", "case", "begin", "do", "?", "where", "end", "in", "of",
      "module", "with", ":", "<:", ":>", "->", "<-", ";", "*", ".", "forall", "infix",
      "left", "right").map(_ ^^ TKeyword);

  def rawId: String = """[a-zA-Z_][a-zA-Z0-9_]*""";
  
  def id: Parser[TId] = rawId.r ^^ TId;

  def exprOp: Parser[TExprOp] = """(\+|-|\*|/|=|<|>)+""".r ^^ TExprOp;

  def int: Parser[TInt] = """[0-9]+""".r ^^ TInt;

  def float: Parser[TFloat] = ("""([0-9]+\.[0-9]*|\.[0-9]+|[0-9]+)""" + exponent).r ^^ TFloat;

  def exponent = """[eE][+\-]?[0-9]+""";

  // TODO better comment syntax, with nestable block comments
  def comment: Parser[Unit] = """//[^\n\r]*\r?\n""".r ~> success(())

  def ws: Parser[Unit] = """[ \t\r\n]*""".r ~> success(());

  def charContent(quote: String, card: String) =
    ("""(\\[btnfr"'\\]|\\u[0-9a-fA-F]{4}|\\[0-3]?[0-7]{1,2}|[^\\""" + quote + "])" + card).r;

  def string: Parser[TString] = "\"" ~> charContent("\"", "*") <~ "\"" ^^ TString;

  def char: Parser[TChar] = "'" ~> charContent("'", "") <~ "'" ^^ TChar;

}
