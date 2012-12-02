
module Resolver where

import qualified Data.Map as Map

import Common
import Syntax

data Global =
  Global
  { gRoot :: [ModDecl]
  , gMods :: Map ModName [ModDecl]
  , gSigs :: Map ModName [SigDecl]
  } deriving (Eq, Ord, Show)

emptyGlobal root = Global root Map.empty Map.empty

data Env =
  Env
  { eMods :: Map BindName ModExpr
  , eSigs :: Map BindName SigExpr
  , eTys  :: Map BindName TyExpr
  , eVals :: Map BindName ValExpr
  } deriving (Eq, Ord, Show)

emptyEnv = Env Map.empty Map.empty Map.empty Map.empty

type ResolveM = ReaderT Env (StateT Global FM)

resolve :: [ModDecl] -> FM Global
resolve = execStateT (runReaderT f emptyEnv) . emptyGlobal
  where
    f = do
      r <- gets gRoot >>= mapM resolveModDecl
      modify $ \g -> g { gRoot = r }

resolveModDecl :: ModDecl -> ResolveM ModDecl
resolveModDecl = undefined

