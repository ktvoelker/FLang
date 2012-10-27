package net.karlv.flang
import scalaz._
import Scalaz._

package object ast extends EqualLow {
  
  type Root = Record[ModDecl];

  type ModExpr = Nothing;
  
  type SigExpr = Nothing;
  
  type Module = Expr[ModExpr, ModDecl];
  
  type Sig = Expr[SigExpr, SigDecl];
  
  type Type = Expr[TyExpr, TyDecl];
  
  type Value = Expr[ValExpr, ValDecl];
  
  implicit def BindNameEqual: Equal[BindName] = equalA;

}