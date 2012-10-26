package net.karlv.flang.ast

case class PatApp(con: Ref, args: List[Pat]) extends Pat {
  
}