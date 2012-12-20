
module Renamer.Sorter where

import Data.Graph
import qualified Data.Set as Set

import Common
import Syntax

{--
 - 1. Sanity check: the B sets are disjoint
 - 2. Make a substitution function and use it to collapse each B set to one Integer
 - 3. Use a comprehension to generate all edges B1 -> B2 where B2 appears in the R set
 -    for node B1
 - 4. Sort the graph topologically
 - 5. Report circularity errors, if prohibited
 - 6. Flatten the SCC values into a list of the decls
 -}
sortDecls :: (Decl a, Show a) => [(a, Set Integer)] -> FM [a]
sortDecls pairs = do
  -- TODO put the sanity check back
  -- Make the referencing graph
  let graph = [(decl, bindKey decl, map Key . Set.toList $ rs) | (decl, rs) <- pairs]
  let sccs = stronglyConnComp graph
  -- Report circularity errors
  mapM_ (report . ECircRef . show . length)
    . filter (any $ not . allowInCycles)
    . map flattenSCC
    . filter isCycle
    $ sccs
  -- Result: the decls, in order
  flattenSCCs <$> pure sccs

isCycle :: SCC a -> Bool
isCycle (AcyclicSCC _) = False
isCycle (CyclicSCC _) = True

twoDisjoint :: (Ord a) => Set a -> Set a -> Bool
twoDisjoint = (Set.null .) . Set.intersection

disjoint :: (Ord a) => [Set a] -> Bool
disjoint ss = all id [twoDisjoint as bs | (as, ka) <- ss', (bs, kb) <- ss', ka /= kb]
  where
    ss' = zip ss [(1 :: Integer)..]

newtype Key = Key Integer deriving (Eq, Ord, Show)

-- TODO: we should be using richer types so the impossible is known to be so
bindKey :: (Decl a) => a -> Key
bindKey = Key . fromJust . listToMaybe . sort . map f . declBindNames
  where
    f (UniqueName n _) = n
    f _ = error "Impossible!"

