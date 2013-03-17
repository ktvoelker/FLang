
module Infix (eliminateInfix) where

import qualified Data.Map as Map

import Common
import Syntax
import Syntax.Traverse

data Fixity = Fixity InfixAssoc Integer 
  deriving (Show)

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
elimExpr = todo
{-
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
-}

elimDecl :: Decl t -> M (Decl t)
elimDecl = todo
{-
    Constraint :: Expr Ty -> TyCompOp -> Expr Ty -> Decl Ty
    ValField   :: BindName -> Expr Ty -> Decl Ty
    TyField    :: BindName -> Maybe TyBound -> Decl Ty
    ModField   :: BindName -> Expr Ty -> Decl Ty
    -- TODO: Combine BindLocal and BindVal when the necessary predicate
    -- (TyTag t ~ Ty) is supported by Template Haskell.
    BindLocal  :: Binding Val -> Decl Val
    BindVal    :: Binding Val -> Decl Mod
    BindMod    :: Binding Mod -> Decl Mod
    BindSig    :: Binding Ty -> Decl Mod
    BindTy     :: Binding Ty -> Decl Mod
    Infix      :: InfixAssoc -> Integer -> [BindName] -> Decl Mod
    Data       :: DataMode -> BindName -> Maybe (Expr Ty) -> Expr Ty -> [Decl Mod]
               -> Decl Mod
-}

elimCaseClause :: CaseClause -> M CaseClause
elimCaseClause = todo
{-
    CaseClause Pat (Expr Val)
-}

elimDo :: [DoElem] -> M [DoElem]
elimDo = todo
{-
    DoLet [Decl Val]
    DoBind Pat (Expr Val)
    DoExpr (Expr Val)
-}

