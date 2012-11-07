package net.karlv.flang.compile
import net.karlv.flang.ast._
import net.karlv.flang._

object ModInterpreter extends Function2[Env[UMod], UMod.Expr, UMod.Expr] {
  
  def apply(env: Env[UMod], expr: UMod.Expr): UMod.Expr = expr match {
    case UMod.Record(decls) => UMod.Record(decls.map(NameResolver(env, _)));
    case UMod.Ref(names) => throw new Error(ENotImpl);
    case _ => throw new Error(EImpossible);
  };

}

object SigInterpreter extends Function2[Env[USig], USig.Expr, USig.Expr] {
  
  def apply(env: Env[USig], expr: USig.Expr): USig.Expr = expr match {
    case USig.Record(decls) => null;
  };
  
}