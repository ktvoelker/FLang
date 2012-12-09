
{-# LANGUAGE TemplateHaskell #-}
module Monad.Accum.Internal where

import Data.Lens.Template

data AccumState s =
  AccumState
  { _asInit  :: s
  , _asAccum :: s
  , _asFold  :: s -> s -> s
  }

makeLenses [''AccumState]

