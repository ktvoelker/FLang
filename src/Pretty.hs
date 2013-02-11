
module Pretty where

import Common
import Pretty.Internal

class (TokenKind b) => Pretty a b | a -> b where
  tokens :: a -> Writer [Token b] ()

class TokenKind a where
  space :: a -> a -> Bool

data Token a = Word String a | LineBreak Int | Depth Int
  deriving (Eq, Ord)

pretty :: (Pretty a b) => a -> String
pretty = format . prepare . execWriter . tokens

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
format = execWriter . evalStateT mShow . initPrettyState

type M = StateT PrettyState (Writer String)

isLineBreak (SLineBreak _) = True
isLineBreak _ = False

-- TODO optimize?
break1 pred xs = let (as, bs) = break pred xs in (as ++ take 1 bs, drop 1 bs)

mShow :: M ()
mShow = do
  ts <- access sInput
  case ts of
    [] -> return ()
    _  -> do
      let (line, ts') = break1 isLineBreak ts
      -- TODO apply strategies to line
      mapM_ naiveShow line
      _ <- sInput ~= ts'
      mShow

naiveShow :: SToken -> M ()
naiveShow (SLineBreak indent) = do
  _ <- sIndent %= (+ indent)
  tell "\n"
naiveShow (SDepth depth) = void $ sDepth %= (+ depth)
naiveShow (SWord xs) = tell xs
naiveShow SSpace = tell " "

