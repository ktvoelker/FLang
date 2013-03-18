
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
incomparableErr f1 b1 f2 b2 =
  Err
  { errType = EFixityMismatch
  , errSourcePos = Nothing -- TODO
  , errName = Nothing
  , errMore = Just $ show ((f1, b1), (f2, b2))
  }

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
unboundErr (b, f) =
  Err
  { errType = EOrphanFixity
  , errSourcePos = Nothing -- TODO
  , errName = Just b
  , errMore = Just $ show f
  }

elimExpr :: Expr t -> M (Expr t)
elimExpr (OpChain _ h ts t) = do
  (bs, is) <- prepare h ts t
  e <- appTree is
  return $ foldr (Lam emptyAnn . (: []) . flip Binder Nothing) e bs
elimExpr e = return e

-- TODO we should identify sections in the prep phase, generate names, and return the
-- generated names in the order they should be bound (outer to inner). Then elimExpr
-- can call appTree on a list that always starts and ends with IA, and then wrap the
-- result in Lams as necessary for the section vars.
--
-- TODO first fix the parser to support right sections (like (3 +))
prepare
  :: Maybe (Expr t)
  -> [(Expr t, Expr t)]
  -> Maybe (Expr t)
  -> M ([BindName], [IN t])
prepare h ts t = do
  h'  <- mapM prepArg $ maybeToList h
  ts' <- mapM (\(o, a) -> sequence [prepOp o, prepArg a]) ts
  t'  <- mapM prepOp $ maybeToList t
  vh  <- case h of
    Nothing -> sequence [lift sectionVar]
    Just _  -> return []
  vt  <- case t of
    Nothing -> return []
    Just _  -> sequence [lift sectionVar]
  return (vh ++ vt, mr vh ++ h' ++ concat ts' ++ t' ++ mr vt)
  where
    prepArg = return . IA
    prepOp (Ref _ n) = do
      f <- asksName n
      case f of
        Nothing -> impossible "Unbound name in prepOp"
        Just f -> return $ IO f n
    prepOp _ = impossible "Unexpected expression in prepOp"
    mr = map $ IA . Ref emptyAnn
    sectionVar = do
      n <- nextUnique
      return
        $ UniqueName emptyAnn n
        $ UniqueInfo
          { _uniqueOrigName  = "!section"
          , _uniqueGenerated = True
          , _uniqueSection   = True
          }

appTree :: [IN t] -> M (Expr t)
appTree [] = impossible "Empty list in appTree"
appTree [IA a] = return a
appTree [IO _ _] = impossible "List of one operator in appTree"
appTree xs = do
  (o, (hs, ts)) <- lift $ pick xs
  case o of
    IO _ n -> do
      args <- sequence [appTree hs, appTree ts]
      return $ App emptyAnn (Ref emptyAnn n) args
    IA _ -> impossible "Split on an argument in appTree"

