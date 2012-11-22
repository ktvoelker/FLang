
module Common
  ( module Common
  , module Prelude
  , module Data.Either
  , module Data.List
  , module Data.Maybe
  , module Data.Ratio
  , Map()
  , Set()
  , Text()
  ) where

import Prelude hiding
  ( String
  , error
  , appendFile
  , getContents
  , getLine
  , interact
  , putStr
  , putStrLn
  , writeFile
  , ReadS
  , read
  , readFile
  , readIO
  , readList
  , readLn
  , readParen
  , reads
  , readsPrec
  , show
  , showChar
  , showList
  , showParen
  , showString
  , shows
  , showsPrec
  )

import Data.Either

import Data.List

import Data.Maybe

import Data.Ratio

import Data.Map
  ( Map()
  )

import Data.Set
  ( Set()
  )

import Data.Text
  ( Text()
  )

#include "Qual.h"

import qualified Prelude as P

error :: Text -> a
error = P.error . Text.unpack

show :: (Show a) => a -> Text
show = Text.pack . P.show

read :: (Read a) => Text -> a
read = P.read . Text.unpack

readMaybe :: (Read a) => Text -> Maybe a
readMaybe = fmap fst . listToMaybe . P.reads . Text.unpack

