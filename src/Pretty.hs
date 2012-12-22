
{-# LANGUAGE UndecidableInstances #-}
module Pretty where

import Common
import Pretty.Internal

class (TokenKind b) => Pretty a b | a -> b where
  tokens :: a -> [Token b]

class TokenKind a where
  space :: a -> a -> Bool

data Token a = Word String a | LineBreak Int | Depth Int
  deriving (Eq, Ord)

data SToken = SWord String | SSpace | SLineBreak Int | SDepth Int
  deriving (Eq, Ord, Show)

pretty :: (Pretty a b) => a -> String
pretty = format . prepare . tokens

prepare :: (TokenKind a) => [Token a] -> [SToken]
prepare [] = []
-- LineBreak ...
prepare (LineBreak n : ts) = SLineBreak n : prepare ts
-- Depth Depth ...
prepare ts@(Depth _ : Depth _ : _) = prepare $ depthCollapse ts
-- Depth ...
prepare (Depth n : ts) = SDepth n : prepare ts
-- Word LineBreak ...
prepare (Word xs _ : LineBreak n : ts) =
  SWord xs : SLineBreak n : prepare ts
-- Word Depth Depth ...
prepare (t@(Word _ _) : ts@(Depth _ : Depth _ : _)) =
  prepare $ t : depthCollapse ts
-- Word Depth Word ...
prepare (Word xs a : Depth n : Word ys b : ts) =
  SWord xs : spaced a b (SDepth n : SWord ys : prepare ts)
-- Word Word ...
prepare (Word xs a : Word ys b : ts) =
  SWord xs : spaced a b (SWord ys : prepare ts)
-- Word Depth ...
prepare (Word xs _ : Depth n : ts) =
  SWord xs : SDepth n : prepare ts
-- Word
prepare [Word xs _] = [SWord xs]

depthCollapse :: [Token a] -> [Token a]
depthCollapse (Depth m : Depth n : ts) = depthCollapse $ Depth (m + n) : ts
depthCollapse ts = ts

spaced :: (TokenKind a) => a -> a -> [SToken] -> [SToken]
spaced a b = case space a b of
  True  -> (SSpace :)
  False -> id

format :: [SToken] -> String
format = evalState (runReaderT mShow initEnv)

type M = ReaderT Env (State [SToken])

mShow :: M String
mShow = undefined

{--
 - Find all the tokens up to the next hard line break. If they don't fit on one line,
 - apply various line-breaking strategies to split them into multiple lines. Each
 - strategy should be given the tokens that need splitting (with the precomputed
 - intervening whitespace), as well as the actual available line length, taking into
 - account the ambient indentation. Each strategy produces a result:
 -
 -   data SplitResult = Success [Line] Score | Failure
 -
 - There may be some strategies that can be applied at a variety of different
 - syntactic depths, which is fine. Just pick the result with the highest score.
 -
 - There could also be some scoring threshold above which we just stop trying
 - alternatives. This especially makes sense if we can sort the strategies into a
 - preferred order.
 -}

