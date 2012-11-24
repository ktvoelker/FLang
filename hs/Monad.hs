
module Monad (CompileError(..), FM(), runFM) where

import Control.Monad.Writer

import Import

data CompileError =
    EUnknown String
  deriving (Eq, Ord, Show)

newtype FM a = FM { getFM :: Writer [CompileError] a }

instance Monad FM where
  return = FM . return
  (FM m) >>= f = FM $ m >>= getFM . f
  fail = FM . fail

instance Functor FM where
  fmap f m = m >>= return . f

runFM :: FM a -> (a, [CompileError])
runFM = runWriter . getFM

