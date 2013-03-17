
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

pick :: [IN t] -> FM (IN t, ([IN t], [IN t]))
pick xs = minimumByM f $ zip xs $ splits xs
  where
    f ((IA _), _) ((IA _), _) = return EQ
    f ((IA _), _) ((IO _ _), _) = return GT
    f ((IO _ _), _) ((IA _), _) = return LT
    f ((IO fa na), _) ((IO fb nb), _) = case cmp fa fb of
      Nothing -> fatal $ incomparableErr fa na fb nb
      Just o -> return o

incomparableErr :: Fixity -> BindName -> Fixity -> BindName -> Err
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
elimExpr (OpChain _ h ts t) = prepare h ts t >>= appTree
elimExpr e = return e

-- TODO we should identify sections in the prep phase, generate names, and return the
-- generated names in the order they should be bound (outer to inner). Then elimExpr
-- can call appTree on a list that always starts and ends with IA, and then wrap the
-- result in Lams as necessary for the section vars.
--
-- TODO first fix the parser to support right sections (like (3 +))
prepare :: Maybe (Expr t) -> [(Expr t, Expr t)] -> Maybe (Expr t) -> M [IN t]
prepare h ts t = do
  h' <- mapM prepArg (maybeToList h)
  ts' <- mapM (\(o, a) -> sequence [prepOp o, prepArg a]) ts
  return $ h' ++ concat ts'
  where
    prepArg = return . IA
    prepOp (Ref _ n) = do
      f <- asksName n
      case f of
        Nothing -> impossible "Unbound name in prepOp"
        Just f -> return $ IO f n
    prepOp _ = impossible "Unexpected expression in prepOp"

appTree :: [IN t] -> M (Expr t)
--   2. The base case is a one-element list, which will always be just a value - so
--   just return that value expression.
--
--   3. The recursive case has to check whether either side of the split comes up
--   empty. If so, it has to generate a section rather than recursing on the empty
--   list. If the left side is empty, generate \x -> o x r; if the right side is empty,
--   generate \x -> o l x (or just o l).
appTree [] = todo
appTree [IA a] = return a

