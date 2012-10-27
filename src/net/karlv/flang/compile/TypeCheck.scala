package net.karlv.flang.compile
import net.karlv.flang.ast._
import scalaz._
import Scalaz._

class TypeChecker(root: Root) {
  
  def numParams(ty: Type): Int = 42; // TODO
  
  def isModuleNamed(name: BindName)(decl: ModDecl): Boolean = decl match {
    case mod: BindModule => mod.binder.id === name && mod.binder.ty.map(numParams).getOrElse(0) === 0;
  }
  
  def run(): Unit = {
    
  };

}