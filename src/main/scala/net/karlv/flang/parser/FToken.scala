package net.karlv.flang.parser

import scalaz._
import Scalaz._

abstract class FToken(override val chars: String) extends Lexicon.Token {}

case class TKeyword(word: String) extends FToken(word) {}

case class TId(val xs: String) extends FToken(xs) {}

case class TExprOp(val xs: String) extends FToken(xs) {}

case class TInt(raw: String) extends FToken(raw) {

  val n = BigInt(raw);

}

case class TFloat(raw: String) extends FToken(raw) {

  val n = BigDecimal(raw);

}

abstract class TChars(raw: String) extends FToken(raw) {

  lazy val decoded: String = {
    val b = new StringBuilder();
    var pos: Int = 0;
    while (pos < raw.length()) {
      raw(pos) match {
        case '\\' =>
          {
            val ch = raw(pos + 1);
            if (ch === 'U' || ch === 'u') {
              b.append(Integer.getInteger(raw.substring(pos + 2, pos + 6), 16).toChar);
              pos += 6;
            } else if (ch >= '0' && ch <= '9') {
              b.append(Integer.getInteger(
                  raw.substring(pos + 1, pos + 4).takeWhile(ch => ch >= '0' && ch <= '7'), 8).toChar);
              pos += 4;
            } else {
              b.append(ch);
              pos += 2;
            }
          };
        case ch => {
          b.append(ch);
          pos += 1;
        };
      };
    }
    b.toString();
  };

}

case class TString(raw: String) extends TChars(raw) {

  lazy val xs = decoded;

}

case class TChar(raw: String) extends TChars(raw) {

  lazy val char = decoded(0);

}
