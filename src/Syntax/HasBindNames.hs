
module Syntax.HasBindNames where

import Common
import Syntax.Types

class HasBindNames a where
  bindNames :: a -> [BindName]

instance HasBindNames BindName where
  bindNames name = [name]

instance HasBindNames (Binder t) where
  bindNames (Binder n _) = [n]

instance HasBindNames (Binding t) where
  bindNames (Binding b _) = bindNames b

instance HasBindNames (Decl t) where
  bindNames (Constraint _ _ _ _) = []
  bindNames (ValField _ n _) = [n]
  bindNames (ModField _ n _) = [n]
  bindNames (TyField _ n _) = [n]
  bindNames (BindLocal _ b) = bindNames b
  bindNames (BindMod _ b) = bindNames b
  bindNames (BindSig _ b) = bindNames b
  bindNames (BindVal _ b) = bindNames b
  bindNames (BindTy _ b) = bindNames b
  bindNames (Infix _ _ _ _) = []
  bindNames (Data _ _ n _ _ ds) = n : concatMap bindNames ds

instance HasBindNames Pat where
  bindNames (PatParams _ ps) = ps >>= bindNames
  bindNames (PatBind _ n) = [n]
  bindNames (PatApp _ _ ps) = ps >>= bindNames
  bindNames (PatLit _ _) = []
  bindNames (PatIgnore _) = []

