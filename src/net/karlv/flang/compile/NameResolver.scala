package net.karlv.flang.compile
import net.karlv.flang.ast._

class NameResolver(root: UMod.Record) {
  
  def run(): Unit = {
    
    root.elems.flatMap(_.childExprs).flatMap(_.descendants).foreach(_ match {
      // TODO iterate over SigRef and TyRef
      // TODO why bother with all this typed nonsense when we could use Product?
      case ref: UMod.Ref => runModRef(ref);
      case bind: BindVal => bind.body.descendants.foreach(_ match {
        case ref: UVal.Ref => runValRef(ref);
      });
    });
    
  };
  
  def runModRef(ref: UMod.Ref): Unit = {
    
  };
  
  def runValRef(ref: UVal.Ref): Unit = {
    
  };

}