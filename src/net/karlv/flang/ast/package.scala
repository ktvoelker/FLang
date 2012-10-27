package net.karlv.flang
import scalaz._
import Scalaz._

package object ast extends EqualLow {
  
  type Root = UMod.Record;
  
  implicit object UVal extends Universe { }
  implicit object UTy extends Universe { }
  implicit object UMod extends Universe { }
  implicit object USig extends Universe { }
  
  implicit def BindNameEqual: Equal[BindName] = equalA;
  implicit def NamespaceEqual: Equal[Namespace] = equalA;

}