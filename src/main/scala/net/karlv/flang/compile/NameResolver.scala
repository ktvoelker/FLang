package net.karlv.flang.compile
import net.karlv.flang.Util._
import net.karlv.flang.ast._

object NameResolver extends Function2[UMod.Env, UMod.Decl, UMod.Decl] {
  
  def apply(env: UMod.Env, decl: UMod.Decl): UMod.Decl = decl match {
    case BindModule(b, e) => BindModule(b, ModInterpreter(env.intoModule(b.id), e));
    case BindSig(b, e) => BindSig(b, SigInterpreter(env.intoUniverse[USig.type](USig), e));
    case _ => decl;
  };
  
}