
{-# LANGUAGE TemplateHaskell #-}
module Annotation.Demo where

import Common

annotate [d|
  data Foo = A Int | B Int String | C { a :: Int } | Int :+ Int
    deriving (Eq, Ord, Show) |]

