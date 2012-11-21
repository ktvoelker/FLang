
module Token where

#include "Common.h"

data Token =
    TKeyword Text
  | TId Text
  | TExprOp Text
  | TInt Integer
  | TFloat Rational
  | TString Text
  | TChar Char
  deriving (Eq, Ord, Show)

