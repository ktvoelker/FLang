package net.karlv.flang

package object ast {

  type ModExpr = Nothing;
  
  type SigExpr = Nothing;
  
  type Module = Expr[ModExpr, ModDecl];
  
  type Sig = Expr[SigExpr, SigDecl];
  
  type Type = Expr[TyExpr, TyDecl];
  
  type Value = Expr[ValExpr, ValDecl];

}