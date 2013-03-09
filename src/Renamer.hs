
module Renamer where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Common
import Syntax
import Syntax.Traverse
import Types

import Renamer.Sorter

type AR = AccumT (Set Integer)

type M' = StateT Global (AR FM)

type M = ReaderT (Env Integer) M'

renameTraversal :: Traversal Integer M'
renameTraversal =
  (emptyTraversal makeScope makeScope)
  { onNameRef    = renameNameRef
  , onNameBind   = renameNameBind
  , onDecl       = renameDecl
  , onExpr       = renameExpr
  }

rename :: Program -> FM Global
rename p =
  evalAccumT (execStateT f $ emptyGlobal p) Set.empty Set.union
  where
    f = do
      result <- gRoot %>>= mapProgram renameTraversal
      lift $ getAccum >>= lift . internal
      return result

allocUnique :: (MonadState Global m) => m Integer
allocUnique = gNextUnique %= (+ 1)

insertRef :: (Monad m) => Integer -> AR m ()
insertRef n = getAccum >>= putAccum . Set.insert n

renameNameFrom :: Lens (Env Integer) (Map BindName Integer) -> BindName -> M BindName
renameNameFrom field n@(BindName a xs) = do
  env <- ask
  let z = Map.lookup n $ env ^. field
  case z of
    Nothing -> do
      lift3
        . report
        $ Err EUnbound (a ^. annSourcePos) (Just n) Nothing
      return n
    Just z -> return $ UniqueName a z xs
renameNameFrom _ n@(UniqueName _ _ _) = return n

renameNameRef :: BindName -> M BindName
renameNameRef n = do
  n' <- renameNameFrom eScope n
  case n' of
    UniqueName _ z _ -> lift2 . insertRef $ z
    _ -> return ()
  return n'

renameNameBind :: BindName -> M BindName
renameNameBind = renameNameFrom eBinds

makeScope :: BindMap a -> M (BindMap Integer)
makeScope = mapM (const allocUnique)

renameSortDecls :: [Decl t] -> M [Decl t]
renameSortDecls = lift3 . sortDecls

renameDecl :: Decl t -> M (Decl t)
renameDecl = branch . return

renameExpr :: Expr t -> M (Expr t)
renameExpr (Record a ds) = Record a <$> renameSortDecls ds
renameExpr (Let a ds e) = Let a <$> renameSortDecls ds <*> pure e
renameExpr e = return e

