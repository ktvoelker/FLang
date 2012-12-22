
{-# LANGUAGE TemplateHaskell #-}
module Pretty.Internal where

import Data.Lens.Template

import Common

data Env =
  Env
  { _eIndent     :: Int
  , _eDepth      :: Int
  , _eLineDepth  :: Int
  , _eLineSize   :: Int
  , _eIndentSize :: Int
  } deriving (Eq, Ord, Show)

initEnv = Env 0 0 0 80 4

makeLenses [''Env]

