package net.karlv.flang

import org.antlr.runtime.tree.CommonTree

package object ast {
  implicit def fromCommonTree(orig: CommonTree) = new KTree(orig);
}