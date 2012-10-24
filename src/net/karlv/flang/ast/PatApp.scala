package net.karlv.flang.ast

case class PatApp(con: IdRef, args: List[Pat]) extends Pat {
  
}