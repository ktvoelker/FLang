package net.karlv.flang.ast

abstract class Type {

}

object Type {
  
  def defaultModuleType: Type = null;
  
  def parse(t: KTree): Type = null;

}