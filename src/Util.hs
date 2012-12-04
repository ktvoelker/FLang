
module Util where

import Import

readMaybe :: (Read a) => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM = (>>= put) . (get >>=) 

(%>>=) :: (MonadState a m) => Lens a b -> (b -> m b) -> m b
(%>>=) lens f = access lens >>= f >>= (lens ~=)

infixr 4 %>>=

