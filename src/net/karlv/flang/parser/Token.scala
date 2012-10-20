package net.karlv.flang.parser

abstract class Token { }

case class TKeyword(word: String) extends Token { }

case class TId(xs: String) extends Token { }

case class TName(xs: List[String]) extends Token { }

case class TExprOp(xs: String) extends Token { }

case class TInt(n: BigInt) extends Token { }

case class TFloat(n: BigDecimal) extends Token { }

case class TString(xs: String) extends Token { }

case class TChar(xs: String) extends Token { }

case object TEndOfInput extends Token { }