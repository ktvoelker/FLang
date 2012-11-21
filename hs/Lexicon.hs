
module Lexicon where

#include "Common.h"
import Text.Parsec (parse)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text (Parser)
import Token

tokenize :: Text -> Text -> [Token]
tokenize name xs = case parse file (Text.unpack name) xs of
  Left err -> error . show $ err
  Right ts -> ts

file :: Parser [Token]
file = undefined

{--
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

--}

