
module Monad.Accum where

import Import
import Monad.Accum.Internal
import Util

newtype AccumT s m a = AccumT { getAccumT :: StateT (AccumState s) m a }

instance (Monad m) => Monad (AccumT s m) where
  return = AccumT . return
  m >>= f = AccumT $ getAccumT m >>= getAccumT . f
  fail = AccumT . fail

instance (Monad m) => Functor (AccumT s m) where
  fmap f x = x >>= return . f

instance MonadTrans (AccumT s) where
  lift = AccumT . lift

getAccum :: (Monad m) => AccumT s m s
getAccum = AccumT $ access asAccum

putAccum :: (Monad m) => s -> AccumT s m ()
putAccum = void . AccumT . (asAccum ~=)

runBranch :: (Monad m) => AccumT s m a -> AccumT s m (a, s)
runBranch m = do
  prev <- getAccum
  AccumT (access asInit) >>= putAccum
  ret  <- m
  more <- getAccum
  fold <- AccumT $ access asFold
  putAccum $ fold prev more
  return (ret, more)

runAccumT :: (Monad m) => AccumT s m a -> s -> (s -> s -> s) -> m (a, s)
runAccumT (AccumT m) s f =
  (>>= return . mapSnd (^. asAccum))
  $ runStateT m
  $ AccumState
    { _asInit  = s
    , _asAccum = s
    , _asFold  = f
    }

accum :: (Monad m) => s -> AccumT s m ()
accum = void . runBranch . putAccum

