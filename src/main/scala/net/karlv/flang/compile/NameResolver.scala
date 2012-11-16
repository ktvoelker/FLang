package net.karlv.flang.compile
import net.karlv.flang.Util._
import net.karlv.flang.ast._

object NameResolver extends Function2[Env[UMod.type], UMod.Decl, UMod.Decl] {
  
  def apply(env: Env[UMod.type], decl: UMod.Decl): UMod.Decl = decl match {
    case BindModule(b, e) =>
      BindModule(b, ModInterpreter(env.intoModule(b.id), e));
    case BindSig(b, e) =>
      BindSig(b, SigInterpreter(env.intoUniverse[USig.type], e));
    case _ => decl;
  };
  
}
