
module Util where

import Import

readMaybe :: (Read a) => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

lift2 :: (MonadTrans t, MonadTrans u, Monad m, Monad (u m)) => m a -> t (u m) a
lift2 = lift . lift

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM = (>>= put) . (get >>=) 

(%>>=) :: (MonadState a m) => Lens a b -> (b -> m b) -> m b
(%>>=) lens f = access lens >>= f >>= (lens ~=)

infixr 4 %>>=

