
module Common
  ( module Common
  , module Import
  , module Monad
  ) where

import Import
import Monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

