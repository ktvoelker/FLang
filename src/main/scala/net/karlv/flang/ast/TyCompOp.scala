package net.karlv.flang.ast

abstract class TyCompOp {}
case object OpSubTy extends TyCompOp {}
case object OpSuperTy extends TyCompOp {}
case object OpEqualTy extends TyCompOp {}
