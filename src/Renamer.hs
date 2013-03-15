
module Renamer where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Common
import Syntax
import Syntax.Traverse

import Renamer.Sorter
import Renamer.Types

type M' = StateT RenamerState FM

type M = ReaderT (Env Integer) M'

renameTraversal :: Traversal Integer M'
renameTraversal =
  (emptyTraversal makeScope (const makeScope))
  { onNameRef    = renameNameRef
  , onNameBind   = renameNameBind
  , onExpr       = renameExpr
  }

rename :: Program -> FM Program
rename = evalStateT (rsProgram %>>= mapProgram renameTraversal) . emptyRenamerState

allocUnique :: (MonadState RenamerState m) => m Integer
allocUnique = rsNextUnique %= (+ 1)

insertRef :: Integer -> M ()
insertRef n = do
  path <- asks (ePath ^$)
  lift
    . focus rsRefs
    . mapM_ (\name -> mapLens name %= Just . maybe Set.empty (Set.insert n))
    $ path

renameNameFrom :: Lens (Env Integer) (Map BindName Integer) -> BindName -> M BindName
renameNameFrom field n@(BindName a xs) = do
  env <- ask
  let z = Map.lookup n $ env ^. field
  case z of
    Nothing -> do
      lift2
        . report
        $ Err EUnbound (a ^. annSourcePos) (Just n) Nothing
      return n
    Just z -> return $ UniqueName a z xs
renameNameFrom _ n@(UniqueName _ _ _) = return n

renameNameRef :: BindName -> M BindName
renameNameRef n = do
  n' <- renameNameFrom eScope n
  case n' of
    UniqueName _ z _ -> insertRef z
    _ -> return ()
  return n'

renameNameBind :: BindName -> M BindName
renameNameBind = renameNameFrom eBinds

makeScope :: BindMap a -> M (BindMap Integer)
makeScope = mapM (const allocUnique)

renameSortDecls :: [Decl t] -> M [Decl t]
renameSortDecls ds = do
  lift
  $ focus rsRefs
  $ mapM (\d -> (d,) . f <$> mapM (access . mapLens) (binds d)) ds
  >>= lift . sortDecls
  where
    f = maybe Set.empty Set.unions . sequence

renameExpr :: Expr t -> M (Expr t)
renameExpr (Record a ds) = Record a <$> renameSortDecls ds
renameExpr (Let a ds e) = Let a <$> renameSortDecls ds <*> pure e
renameExpr e = return e

