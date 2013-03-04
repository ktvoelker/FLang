
module Infix (eliminateInfix) where

import qualified Data.Map as Map

import Common
import Syntax
import Types

type M = ReaderT InfixEnv FM

data Fixity = Fixity InfixAssoc Integer 
  deriving (Show)

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
  Nothing -> fatal $ fixityErr p1 p2
  Just o -> return o

fixityErr :: (BindName, Fixity) -> (BindName, Fixity) -> Err
fixityErr = todo

type InfixEnv = Map BindName Fixity

eliminateInfix :: Global -> FM Global
eliminateInfix = (gRoot ^%%= flip runReaderT Map.empty . elimExpr)

-- TODO Check that infix declarations are always in the same Rec as the binding
-- TODO Eliminate OpChain expressions according to fixity, using defaults as needed

elimExpr :: Expr t -> M (Expr t)
elimExpr = todo pick
{-
    Lam       :: [Binder t] -> Expr t -> Expr t
    App       :: Expr t -> [Expr t] -> Expr t
    Record    :: [Decl t] -> Expr t
    Ref       :: BindName -> Expr t
    UniqueRef :: Integer -> Expr t
    Member    :: Expr t -> BindName -> Expr t
    OpChain   :: Maybe (Expr t) -> [(Expr t, Expr t)] -> Expr t
    Let       :: [Decl t] -> Expr t -> Expr t
    ToDo      :: Expr t
    LamCase   :: [CaseClause] -> Expr Val
    Case      :: Expr Val -> [CaseClause] -> Expr Val
    Do        :: [DoElem] -> Expr Val
    Lit       :: Lit t -> Expr t
-}

-- elimDecl :: Decl t -> M (Decl t)
-- elimDecl = todo


