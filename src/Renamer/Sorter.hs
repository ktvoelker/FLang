
module Renamer.Sorter where

import Data.Graph
import qualified Data.Set as Set

import Common
import Syntax (Decl(allowInCycles))
import Types

{--
 - 1. Sanity check: the B sets are disjoint
 - 2. Make a substitution function and use it to collapse each B set to one Integer
 - 3. Use a comprehension to generate all edges B1 -> B2 where B2 appears in the R set
 -    for node B1
 - 4. Sort the graph topologically
 - 5. Report circularity errors, if prohibited
 - 6. Flatten the SCC values into a list of the decls
 -}
sortDecls :: (Decl a, Show a) => [(a, BR)] -> FM [a]
sortDecls ds = do
  let brs = map snd ds
  let bs = map (brBinds ^$) brs
  -- Check that the binding sets are disjoint
  when (not . disjoint $ bs)
    . report
    . EInternal
    $ "Non-disjoint binding sets: " ++ show bs
  -- Make the referencing graph
  let graph = [(decl, bindKey (br ^. brBinds), refsKeys brs br) | (decl, br) <- ds]
  let sccs = stronglyConnComp graph
  report . EInternal . show . map (\(_, bs, rs) -> (bs, rs)) $ graph
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

bindKey :: Set Integer -> Key
bindKey = fromJust . fmap (Key . fst) . Set.minView

refKey :: [BR] -> Integer -> Maybe Key
refKey bss n =
  fmap bindKey
  . listToMaybe
  . filter (Set.member n)
  . map (brBinds ^$)
  $ bss

refsKeys :: [BR] -> BR -> [Key]
refsKeys bss = catMaybes . Set.toList . Set.map (refKey bss) . (brRefs ^$)

