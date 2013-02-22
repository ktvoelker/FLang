
module Syntax.HasBindNames where

import Common
import Syntax.Types

class HasBindNames a where
  bindNames :: a -> [BindName]

instance HasBindNames BindName where
  bindNames name = [name]

instance HasBindNames Binder where
  bindNames (Binder n _) = [n]

instance HasBindNames (Binding e) where
  bindNames (Binding b _) = bindNames b

instance HasBindNames ModDecl where
  bindNames (BindMod b) = bindNames b
  bindNames (BindSig b) = bindNames b
  bindNames (BindVal b) = bindNames b
  bindNames (BindTy b) = bindNames b
  bindNames (Data _ n _ _ ds) = n : concatMap bindNames ds
  bindNames (Infix _ _ _) = []

instance HasBindNames SigDecl where
  bindNames (SigVal n _) = [n]
  bindNames (SigTy n _) = [n]
  bindNames (SigMod n _) = [n]

instance HasBindNames ValDecl where
  bindNames (BindLocalVal b) = bindNames b

instance HasBindNames TyDecl where
  bindNames = const []

instance HasBindNames Pat where
  bindNames (PatParams ps) = ps >>= bindNames
  bindNames (PatBind n) = [n]
  bindNames (PatApp _ ps) = ps >>= bindNames
  bindNames _ = []

