
module Renamer.Sorter where

--import Data.Graph

import Common
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
sortDecls :: Bool -> [(a, BR)] -> FM [a]
sortDecls = undefined

