package net.karlv.flang.parser

abstract class FToken(override val chars: String) extends Lexicon.Token { }

case class TKeyword(word: String) extends FToken(word) { }

case class TId(val xs: String) extends FToken(xs) { }

case class TName(raw: String) extends FToken(raw) {
  
  val xs: List[String] = raw.split('.').toList;

}

case class TExprOp(val xs: String) extends FToken(xs) { }

case class TInt(raw: String) extends FToken(raw) {
  
  val n = BigInt(raw);
  
}

case class TFloat(raw: String) extends FToken(raw) {
  
  val n = BigDecimal(raw);

}

case class TString(val xs: String) extends FToken(xs) { }

case class TChar(val xs: String) extends FToken(xs) { }
