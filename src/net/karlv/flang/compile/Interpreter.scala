package net.karlv.flang.compile
import net.karlv.flang.ast._
import net.karlv.flang._

object ModInterpreter extends Function2[UMod.Env, UMod.Expr, UMod.Expr] {
  
  def apply(env: UMod.Env, expr: UMod.Expr): UMod.Expr = expr match {
    case UMod.Record(decls) => UMod.Record(decls.map(NameResolver(env, _)));
    case UMod.Ref(names) => throw new Error(ENotImpl);
    case _ => throw new Error(EImpossible);
  };

}

object SigInterpreter extends Function2[USig.Env, USig.Expr, USig.Expr] {
  
  def apply(env: USig.Env, expr: USig.Expr): USig.Expr = expr match {
    case USig.Record(decls) => null;
  };
  
}