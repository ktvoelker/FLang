package net.karlv.flang

object Util {
  
  def transitiveClosure[T](op: T => List[T])(root: T): List[T] = root :: op(root).flatMap(transitiveClosure(op));

}