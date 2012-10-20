package net.karlv.flang

package object ast {
  
  type ModPrim = GenModPrim[Decl];

  type SigPrim = GenModPrim[SigDecl];

}