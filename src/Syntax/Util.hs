
module Syntax.Util where

import Common
import Syntax.Types

namespace :: BindName -> Namespace
namespace (BindName _ (x : _)) | isUpper x = NsTys
namespace _ = NsValues

allowInCycles :: Decl t -> Bool
allowInCycles (BindLocal _ _) = True
allowInCycles (BindVal _ _) = True
allowInCycles (Data _ _ _ _ _) = True
allowInCycles _ = False

binderName :: Binder t -> BindName
binderName (Binder n _) = n

bindingName :: Binding t -> BindName
bindingName (Binding b _) = binderName b

class Binds a where
  binds :: a -> [BindName]

instance Binds (Binder t) where
  binds = (: []) . binderName

instance Binds (Binding t) where
  binds = (: []) . bindingName

instance Binds (Decl t) where
  binds (Constraint _ _ _ _) = []
  binds (ValField _ n _) = [n]
  binds (ModField _ n _) = [n]
  binds (TyField _ n _) = [n]
  binds (BindLocal _ b) = [bindingName b]
  binds (BindMod _ b) = [bindingName b]
  binds (BindSig _ b) = [bindingName b]
  binds (BindVal _ b) = [bindingName b]
  binds (BindTy _ b) = [bindingName b]
  binds (Infix _ _ _ _) = []
  binds (Data _ _ n _ _) = [n]

instance Binds Pat where
  binds (PatParams _ ps) = ps >>= binds
  binds (PatBind _ n) = [n]
  binds (PatApp _ _ ps) = ps >>= binds
  binds (PatLit _ _) = []
  binds (PatIgnore _) = []

