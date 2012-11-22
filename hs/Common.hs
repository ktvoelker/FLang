
module Common
  ( module Common
  , module Data.Char
  , module Data.Either
  , module Data.List
  , module Data.Map
  , module Data.Maybe
  , module Data.Ratio
  , module Data.Set
  ) where

import Data.Char

import Data.Either

import Data.List

import Data.Map
  ( Map()
  )

import Data.Maybe

import Data.Ratio

import Data.Set
  ( Set()
  )

readMaybe :: (Read a) => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

