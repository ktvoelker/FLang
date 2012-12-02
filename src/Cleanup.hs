
module Cleanup where

import Common
import Syntax

-- TODO put this into FM so we can report errors
cleanup :: [ModDecl] -> [ModDecl]
cleanup = transformBi cleanupOpChain

cleanupOpChain (OpChain x xs) = unreg . f . regHead x . map (mapFst Just) $ xs
  where
    regHead Nothing = id
    regHead (Just e) = ((Nothing, e) :)
    f [] = []
    -- TODO report error
    f xs@((Just (Ref (BindName ".")), _) : _) = xs
    f ((o1, e1) : xs@((Just (Ref (BindName "."))
cleanupOpChain e = e
