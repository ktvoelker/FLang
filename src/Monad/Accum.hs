
{-# LANGUAGE UndecidableInstances #-}
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

instance (Monad m, MonadReader r m) => MonadReader r (AccumT a m) where
  ask = lift ask
  local f m = m >>= lift . local f . return
  reader = lift . reader

instance (Monad m, MonadState s m) => MonadState s (AccumT a m) where
  get = lift get
  put = lift . put
  state = lift . state

class (Monad m) => MonadAccum a m | m -> a where
  getAccumState :: m (AccumState a)
  putAccum      :: a -> m ()

instance (Monad m) => MonadAccum a (AccumT a m) where
  getAccumState = AccumT get
  putAccum = void . AccumT . (asAccum ~=)

instance (Monad m, MonadAccum a m) => MonadAccum a (ReaderT r m) where
  getAccumState = lift getAccumState
  putAccum = lift . putAccum

instance (Monad m, MonadAccum a m) => MonadAccum a (StateT s m) where
  getAccumState = lift getAccumState
  putAccum = lift . putAccum

getAccumField :: (Monad m, MonadAccum a m) => Lens (AccumState a) b -> m b
getAccumField f = getAccumState >>= return . (f ^$)

getAccum :: (Monad m, MonadAccum a m) => m a
getAccum = getAccumField asAccum

branch :: (Monad m, MonadAccum a m) => m b -> m (b, a)
branch m = do
  prev <- getAccum
  getAccumField asInit >>= putAccum
  ret  <- m
  more <- getAccum
  fold <- getAccumField asFold
  putAccum $ fold prev more
  return (ret, more)

accum :: (Monad m, MonadAccum a m) => a -> m ()
accum = branch . putAccum >=> const (return ())

runAccumT :: (Monad m) => AccumT s m a -> s -> (s -> s -> s) -> m (a, s)
runAccumT (AccumT m) s f =
  (>>= return . mapSnd (^. asAccum))
  $ runStateT m
  $ AccumState
    { _asInit  = s
    , _asAccum = s
    , _asFold  = f
    }

evalAccumT m s f = runAccumT m s f >>= return . fst

execAccumT m s f = runAccumT m s f >>= return . snd

