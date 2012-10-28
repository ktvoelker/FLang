package net.karlv.flang.compile
import net.karlv.flang.ast._
import scalaz._
import Scalaz._

class TypeChecker(root: UMod.Record) {
  
  def numParams(ty: UTy.Expr): Int = 42; // TODO
  
  def isModuleNamed(name: BindName)(decl: ModDecl): Boolean = decl match {
    case mod: BindModule => mod.binder.id === name && mod.binder.ty.map(numParams).getOrElse(0) === 0;
    case _ => false;
  }
  
  def run(): Unit = {
    
  };

}