
module Monad (ErrType(..), Err(..), fatal, report, internal, FM(), runFM) where

import Control.Monad.Writer
import Text.Parsec.Pos (SourcePos())

import Import
import Syntax.Types (BindName())
import Util

data ErrType = EUnknown | ELexer | EParser | EInternal | EUnbound | ECircRef
  deriving (Eq, Ord, Enum, Bounded, Show)

data Err =
  Err
  { errType      :: ErrType
  , errSourcePos :: Maybe SourcePos
  , errName      :: Maybe BindName
  , errMore      :: Maybe String
  } deriving (Show)

newtype FM a = FM { getFM :: Writer [Err] (Maybe a) }

fatal :: Err -> FM a
fatal = (>> mzero) . report

report :: Err -> FM ()
report = tell . (: [])

internal :: (Show a) => a -> FM ()
internal = report . Err EInternal Nothing Nothing . Just . show

instance Monad FM where
  return = FM . return . Just
  (FM m) >>= f = FM $ m >>= maybe (return Nothing) (getFM . f)
  fail = FM . fail

instance Functor FM where
  fmap f m = m >>= return . f

instance Applicative FM where
  pure = return
  (<*>) = liftM2 ($)

instance MonadPlus FM where
  mzero = FM $ return Nothing
  mplus (FM a) (FM b) = FM $ a >>= maybe b (return . Just)

instance MonadWriter [Err] FM where
  writer = FM . writer . mapFst Just
  tell = FM . fmap Just . tell
  listen (FM m) = FM $ listen m >>= uncurry (flip f)
    where
      f w = maybe (return Nothing) (return . Just . (,w))
  pass (FM m) = FM . pass . fmap f $ m
    where
      f Nothing = (Nothing, id)
      f (Just (a, f)) = (Just a, f)

runFM :: FM a -> (Maybe a, [Err])
runFM = runWriter . getFM

