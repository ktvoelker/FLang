package net.karlv.flang.ast

case class Name(val name: String) {
  
  val namespace: Namespace = if (name.charAt(0).isUpper) NsTypes else NsValues;

}

abstract class Namespace { }
case object NsValues extends Namespace { }
case object NsTypes extends Namespace { }
