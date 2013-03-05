
module Infix (eliminateInfix) where

import qualified Data.Map as Map

import Common
import Syntax
import Types

type M = ReaderT InfixEnv FM

data Fixity = Fixity InfixAssoc Integer 
  deriving (Show)

defFixity = Fixity InfixLeft 100

cmp :: Fixity -> Fixity -> Maybe Ordering
cmp (Fixity a1 n1) (Fixity a2 n2) =
  if n1 == n2
  then case (a1, a2) of
    (InfixLeft, InfixLeft) -> Just LT
    (InfixRight, InfixRight) -> Just GT
    _ -> Nothing
  else Just $ compare n1 n2

minimumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
minimumByM = f $ error "Empty list"
  where
    f acc _ [] = return acc
    f acc c (x : xs) = do
      o <- c acc x
      case o of
        LT -> f x c xs
        _ -> f acc c xs

pick :: [(BindName, Fixity)] -> FM BindName
pick = (fmap fst .) $ minimumByM $ \p1@(_, f1) p2@(_, f2) -> case cmp f1 f2 of
  Nothing -> fatal $ incomparableErr p1 p2
  Just o -> return o

incomparableErr :: (BindName, Fixity) -> (BindName, Fixity) -> Err
incomparableErr = todo

type InfixEnv = Map BindName Fixity

eliminateInfix :: Global -> FM Global
eliminateInfix = (gRoot ^%%= flip runReaderT Map.empty . elimExpr)

with :: (MonadReader InfixEnv m) => [(BindName, Fixity)] -> m a -> m a
with = local . Map.union . Map.fromList

findFixity :: [Decl t] -> BindName -> Fixity
findFixity = todo

fixities :: Decl t -> [(BindName, Fixity)]
fixities = todo findFixity

unboundErr :: (BindName, Fixity) -> Err
unboundErr = todo

withDecls :: [Decl t] -> M a -> M a
withDecls ds m = mapM_ (lift . report . unboundErr) bad >> with pairs m
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

elimExpr :: Expr t -> M (Expr t)
elimExpr (Lam a bs e) =
  Lam a bs <$> with (zip (map binderName bs) (repeat defFixity)) (elimExpr e)
elimExpr (App a fn args) = App a <$> elimExpr fn <*> mapM elimExpr args
elimExpr (Record a ds) = Record a <$> mapM elimDecl ds
elimExpr e@(Ref _ _) = return e
elimExpr (Member a e n) = Member a <$> elimExpr e <*> pure n
elimExpr (OpChain _ _ _) = todo pick
elimExpr (Let a ds body) =
  withDecls ds $ Let a <$> mapM elimDecl ds <*> elimExpr body
elimExpr e@(ToDo _) = return e
elimExpr (LamCase a cs) = LamCase a <$> mapM elimCaseClause cs
elimExpr (Case a scru cs) = Case a <$> elimExpr scru <*> mapM elimCaseClause cs
elimExpr (Do a es) = Do a <$> elimDo es
elimExpr e@(Lit _ _) = return e

elimDecl :: Decl t -> M (Decl t)
elimDecl = todo

elimCaseClause :: CaseClause -> M CaseClause
elimCaseClause = todo

elimDo :: [DoElem] -> M [DoElem]
elimDo = todo

