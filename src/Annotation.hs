
{-# LANGUAGE TemplateHaskell #-}
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
annotate = (>>= return . concat) . (>>= mapM annotate1)

annotate1 :: Dec -> Q [Dec]
annotate1 (DataD cxt name tvs cons dvs) = do
    cons'  <- mapM annotateCon cons
    smarts <- mapM mkSmartCon cons
    inst   <- mkAnnInst name (length tvs) cons
    return $ inst : DataD cxt name tvs cons' dvs : smarts
annotate1 (NewtypeD cxt name tvs con dvs) = do
    con'  <- annotateCon con
    smart <- mkSmartCon con
    inst  <- mkAnnInst name (length tvs) [con]
    return [inst, NewtypeD cxt name tvs con' dvs, smart]
annotate1 d = return [d]

annotateCon :: Con -> Q Con
annotateCon (NormalC name tys) = return $ NormalC name $ (NotStrict, ConT ''Ann) : tys
annotateCon (RecC name tys) =
  RecC name . (: tys) . (, NotStrict, ConT ''Ann) <$> newName "_ann"
annotateCon c@(InfixC _ _ _) = return c
annotateCon (ForallC tvs cxt con) = ForallC tvs cxt <$> annotateCon con

mkSmartCon :: Con -> Q Dec
mkSmartCon = undefined

mkAnnInst :: Name -> Int -> [Con] -> Q Dec
mkAnnInst = undefined

class (Monad m) => MonadSourcePos m where
  getSourcePos :: m SourcePos

locate :: (Annotated a, MonadSourcePos m) => m a -> m a
locate m = do
  pos <- getSourcePos
  ret <- m
  return $ (annSourcePos . annotationLens) ^= Just pos $ ret

