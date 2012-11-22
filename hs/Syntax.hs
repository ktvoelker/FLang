
module Syntax where

data ModDecl =
    BindModule ModHeader [ModDecl]
  | BindSig SigHeader [SigDecl]
  deriving (Eq, Ord, Show)

