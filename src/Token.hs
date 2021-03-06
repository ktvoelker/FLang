
module Token where

import Common

data Token =
    TKeyword String
  | TId String
  | TExprOp String
  | TInt Integer
  | TFloat Rational
  | TString String
  | TChar Char
  deriving (Eq, Ord, Show)

