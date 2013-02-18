
{-# LANGUAGE TemplateHaskell #-}
module Annotation.Internal where

import Import
import Data.Lens.Template
import Text.Parsec.Pos (SourcePos())

data Ann =
  Ann
  { _annSourcePos :: Maybe SourcePos
  } deriving (Eq, Ord, Show)

emptyAnn :: Ann
emptyAnn = Ann Nothing

makeLenses [''Ann]

