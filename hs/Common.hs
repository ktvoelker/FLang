
module Common
  ( module Common
  , module Prelude
  , module Data.List
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
  , Read()
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

import Data.List

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

