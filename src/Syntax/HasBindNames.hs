
module Syntax.HasBindNames where

import Common
import Syntax.Types

class HasBindNames a where
  bindNames :: a -> [BindName]

instance HasBindNames No where
  bindNames _ = []

instance HasBindNames BindName where
  bindNames name = [name]

instance HasBindNames (Binder k) where
  bindNames (Binder n _) = [n]

instance HasBindNames (Binding k) where
  bindNames (Binding b _) = bindNames b

instance HasBindNames ModDecl where
  bindNames (BindMod _ b) = bindNames b
  bindNames (BindSig _ b) = bindNames b
  bindNames (BindVal _ b) = bindNames b
  bindNames (BindTy _ b) = bindNames b
  bindNames (Data _ _ n _ _ ds) = n : concatMap bindNames ds
  bindNames (Infix _ _ _ _) = []

instance HasBindNames ValDecl where
  bindNames (BindLocalVal _ b) = bindNames b

instance HasBindNames TyDecl where
  bindNames (Constraint _ _ _ _) = []
  bindNames (ValField _ n _) = [n]
  bindNames (ModField _ n _) = [n]
  bindNames (TyField _ n _) = [n]

instance HasBindNames Pat where
  bindNames (PatParams _ ps) = ps >>= bindNames
  bindNames (PatBind _ n) = [n]
  bindNames (PatApp _ _ ps) = ps >>= bindNames
  bindNames _ = []

