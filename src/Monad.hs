
module Monad (CompileError(..), fatal, report, FM(), runFM) where

import Control.Monad.Writer
import Text.Parsec (ParseError())

import Import
import Util

data CompileError =
    EUnknown String
  | ELexer ParseError
  | EParser ParseError
  | EInternal String
  deriving (Show)

newtype FM a = FM { getFM :: Writer [CompileError] (Maybe a) }

fatal :: CompileError -> FM a
fatal = (>> mzero) . report

report :: CompileError -> FM ()
report = tell . (: [])

instance Monad FM where
  return = FM . return . Just
  (FM m) >>= f = FM $ m >>= maybe (return Nothing) (getFM . f)
  fail = FM . fail

instance Functor FM where
  fmap f m = m >>= return . f

instance MonadPlus FM where
  mzero = FM $ return Nothing
  mplus (FM a) (FM b) = FM $ a >>= maybe b (return . Just)

instance MonadWriter [CompileError] FM where
  writer = FM . writer . mapFst Just
  tell = FM . fmap Just . tell
  listen (FM m) = FM $ listen m >>= uncurry (flip f)
    where
      f w = maybe (return Nothing) (return . Just . (,w))
  pass (FM m) = FM . pass . fmap f $ m
    where
      f Nothing = (Nothing, id)
      f (Just (a, f)) = (Just a, f)

runFM :: FM a -> (Maybe a, [CompileError])
runFM = runWriter . getFM

