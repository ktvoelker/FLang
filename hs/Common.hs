
module Common
  ( module Prelude
  , module Data.List
  , Map()
  , Set()
  , Text()
  ) where

import Prelude hiding
  ( String
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
  , Show()
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

