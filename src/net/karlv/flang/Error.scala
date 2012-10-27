package net.karlv.flang

class Error(val ty: ErrorType, val src: ErrorSource) extends Exception(ty + " at " + src) {
  
  def this(ty: ErrorType) = this(ty, UnknownSource);

}

trait ErrorSource { }

object UnknownSource extends ErrorSource {
  
  override def toString() = "unknown location";

}

trait Pos extends ErrorSource {

  val file: String;

  val line: Int;

  val col: Int;
  
  override def toString() = file + ":" + line + ":" + col;

}

abstract class ErrorType {
  
  override def toString() = getClass().getName();
  
}

case object ENotImpl extends ErrorType { }
