package net.karlv.flang

import scala.collection.mutable

class TreeFormatter(tree: Any) {
  
  lazy val str = {
    output(tree);
    b.toString();
  };
    
  private val b = new StringBuilder();
  
  private val stk = new mutable.Stack[Int]();
  
  private def indent(n: Int) = n match {
    case 0 => "    ";
    case _ => " |  ";
  };
  
  private def descend(children: Int): Unit = {
    stk.push(children);
  };
  
  private def output(x: Any): Unit = {
    if (!stk.isEmpty) {
      stk.tail.reverse.map(indent).foreach(b.append);
      b.append(" +--");
      val n = stk.pop();
      stk.push(n - 1);
    }
    x match {
      case xs: List[_] => {
        val n = xs.length;
        b.append("List of ");
        b.append(n);
        b.append("\n");
        descend(n);
        xs.foreach(output);
      };
      case prod: Product => {
        val n = prod.productArity;
        b.append(prod.getClass().getName());
        b.append("\n");
        descend(n);
        prod.productIterator.foreach(output);
      };
      case _ => {
        b.append(x.toString());
        b.append("\n");
      };
    };
    if (!stk.isEmpty) {
      if (stk.top == 0) {
        stk.pop();
      }
    }
  };

}