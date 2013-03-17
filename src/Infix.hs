
module Infix (eliminateInfix) where

import qualified Data.Map as Map

import Common
import Infix.Types
import Syntax
import Syntax.Traverse

type M = ReaderT (Env Fixity) FM

defFixity = Fixity InfixLeft 100

cmp :: Fixity -> Fixity -> Maybe Ordering
cmp (Fixity a1 n1) (Fixity a2 n2) =
  if n1 == n2
  then case (a1, a2) of
    (InfixLeft, InfixLeft) -> Just LT
    (InfixRight, InfixRight) -> Just GT
    _ -> Nothing
  else Just $ compare n1 n2

pick :: [IN] -> FM (IN, ([IN], [IN]))
pick xs = minimumByM f $ zip xs $ splits xs
  where
    f a b = case cmp (inFixity ^$ a') (inFixity ^$ b') of
      Nothing -> fatal $ incomparableErr a' b'
      Just o -> return o
      where
        a' = fst a
        b' = fst b

incomparableErr :: IN -> IN -> Err
incomparableErr = todo

elimTraversal :: Traversal Fixity FM
elimTraversal =
  (emptyTraversal (makeScope []) makeScope)
  { onExpr       = elimExpr
  }

makeScope :: [Decl t] -> BindMap a -> M (BindMap Fixity)
makeScope ds _ = do
  mapM_ (lift . report . unboundErr) bad
  return $ Map.fromList pairs
  where
    bs = concatMap binds ds
    defs, good, bad, pairs :: [(BindName, Fixity)]
    -- Every binding paired with the default fixity.
    defs = zip bs $ repeat defFixity
    -- Every binding that has a declared fixity paired with that fixity.
    (good, bad) = partition ((`elem` bs) . fst) $ concatMap fixities ds
    -- Map.fromList takes the last list element with a particular key, so the list
    -- of defaults has to precede the list of declared fixities.
    pairs = defs ++ good

eliminateInfix :: Program -> FM Program
eliminateInfix = mapProgram elimTraversal

fixities :: Decl t -> [(BindName, Fixity)]
fixities (Infix _ a n names) = map (, Fixity a n) names
fixities _ = []

unboundErr :: (BindName, Fixity) -> Err
unboundErr = todo

elimExpr :: Expr t -> M (Expr t)
-- TODO make the IN list, pick the place to split at, recurse on the two halves,
-- and put them together as the arguments to an App node with the op as the function
--
-- TODO think a bit about the various edge cases that will make the IN structure
-- difficult to work with - there's the section case and the full application case,
-- for one thing
elimExpr (OpChain _ _ _) = todo pick
elimExpr e = return e

