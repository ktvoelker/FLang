
{-# LANGUAGE TemplateHaskell #-}
module Pretty.Internal where

import Data.Lens.Template

import Common

data SToken = SWord String | SSpace | SLineBreak Int | SDepth Int
  deriving (Eq, Ord, Show)

data PrettyState =
  PrettyState
  { _sIndent     :: Int -- ^ The indentation level
  , _sDepth      :: Int -- ^ The depth in the tree
  , _sLineDepth  :: Int -- ^ The depth in the tree at the beginning of the line
  , _sLineSize   :: Int -- ^ The maximum length of one output line
  , _sIndentSize :: Int -- ^ The number of spaces per indentation level
  , _sInput      :: [SToken] -- ^ The remaining tokens
  } deriving (Eq, Ord, Show)

initPrettyState = PrettyState 0 0 0 80 4

makeLenses [''PrettyState]

