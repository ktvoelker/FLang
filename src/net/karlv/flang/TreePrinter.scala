package net.karlv.flang

object TreePrinter {
  
  val indent = " |  ";
  
  val branch = " +--";
  
  /**
   * The output doesn't look very good.
   * 
   * The key to better output is to pass an emptier prefix
   * to your last item.
   * 
   * Try a new (somewhat more imperative) approach:
   * 
   * 1. Have a current TreeState that is passed along via implicits.
   * 2. Items output themselves to the TreeState, and it handles indentation and layout.
   * 3. Before recursing on its children, the parent tells the tree state that it is descending,
   *    and how many children there will be. Thus, there's no need for the parent to take special action
   *    after descending, and the TreeState can format the first and last children differently, as needed.
   * 4. As another bonus, the TreeState can use a StringBuilder for better performance.
   */
  
  def showTree(t: Any): String = showAny("", "")(t);
    
  private def showAny(prefix: String, branchPrefix: String)(t: Any): String =
    branchPrefix + (t match {
      case Nil => "List (empty)\n";
      case xs: List[_] => showList(xs, prefix);
      case p: Product => showProduct(p, prefix);
      case _ => t.toString() + "\n";
    });
  
  private def showItems(xs: List[Any], show: Any => String): String = new String(xs.map(show).flatten.toArray);
  
  private def showList(t: List[_], prefix: String): String =
    "List\n" + prefix + indent + "\n" + showItems(t, showAny(prefix + indent, prefix + branch)) + prefix + "\n";
  
  private def showProduct(t: Product, prefix: String): String =
    t.productPrefix + "\n" + prefix + indent + "\n" +
        showItems(t.productIterator.toList, showAny(prefix + indent, prefix + branch)) + prefix + "\n";

}