package net.karlv.flang

object Util {
  
  def transitiveClosure[T](op: T => List[T])(root: T): List[T] =
    root :: op(root).flatMap(transitiveClosure(op));
  
  def collect[T](f: Product => Option[T])(p: Product): List[T] =
    f(p).toList ++ p.productIterator.flatMap(_ match {
      case p: Product => collect(f)(p);
      case _ => Nil;
    });
  
  def traverse(f: Product => Unit)(p: Product): Unit =
    collect(f.andThen(_ => None))(p).andThen(_ => ());
  
  def dyn[A <: Product, B](zero: B)(f: A => B)(implicit m: Manifest[A]): (Product => B) =
    p => if (p.getClass() == m.erasure) f(p.asInstanceOf[A]) else zero;
    
  def dyn_[A <: Product](f: A => Unit)(implicit m: Manifest[A]): (Product => Unit) = dyn(())(f);

}