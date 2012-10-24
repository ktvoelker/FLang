package net.karlv.flang.ast

abstract class Pat {

}

case class PatApp(con: IdRef, args: List[Pat]) extends Pat {
  
}

case class PatBind(id: IdDecl) extends Pat {
  
}

case object PatAny extends Pat {
  
}

case class PatInt(n: BigInt) extends Pat {
  
}

case class PatString(xs: String) extends Pat {
  
}

case class PatChar(char: Char) extends Pat {
  
}
