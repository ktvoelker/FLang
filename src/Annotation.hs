
module Annotation
  ( module Annotation
  , module Annotation.Internal
  ) where

import Import
import Annotation.Internal

import Language.Haskell.TH
import Text.Parsec.Pos (SourcePos())

class Annotated a where
  annotationLens :: Lens a Ann

-- | Modify the given 'data' or 'newtype' declarations to have an extra value parameter
--   of type Ann at the end, provide instances of Annotated for the declared types, and
--   provide smart constructors with names starting with 'mk' that take all parameters
--   of the real constructor except the Ann, plus emptyAnn at the end. Constructors
--   declared as records will have a randomly-generated unique name for the Ann field.
annotate :: Q [Dec] -> Q [Dec]
annotate = undefined

class (Monad m) => MonadSourcePos m where
  getSourcePos :: m SourcePos

locate :: (Annotated a, MonadSourcePos m) => m a -> m a
locate m = do
  pos <- getSourcePos
  ret <- m
  return $ (annSourcePos . annotationLens) ^= Just pos $ ret

