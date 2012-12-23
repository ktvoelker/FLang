
{-# LANGUAGE TemplateHaskell #-}
module Pretty.Internal where

import Data.Lens.Template

import Common

data SToken = SWord String | SSpace | SLineBreak Int | SDepth Int
  deriving (Eq, Ord, Show)

data PrettyState =
  PrettyState
  { _sIndent     :: Int
  , _sDepth      :: Int
  , _sLineDepth  :: Int
  , _sLineSize   :: Int
  , _sIndentSize :: Int
  , _sInput      :: [SToken]
  } deriving (Eq, Ord, Show)

initPrettyState = PrettyState 0 0 0 80 4

makeLenses [''PrettyState]

