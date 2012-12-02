
module Resolver where

import qualified Data.Map as Map

import Common
import Syntax

type EnvMap e = Map [BindName] e

data Global =
  Global
  { root :: [ModDecl]
  , mods :: EnvMap [ModDecl]
  , sigs :: EnvMap [SigDecl]
  } deriving (Eq, Ord, Show)

emptyGlobal root = Global root Map.empty Map.empty

data Env e =
  Env
  { curMod :: [BindName]
  , locals :: EnvMap e
  } deriving (Eq, Ord, Show)

emptyEnv = Env [] Map.empty

-- TODO we need the env to have mod, sig, ty, and expr locals
type ResolveM = ReaderT (Env ModExpr) (StateT Global FM)

{-
resolve :: [ModDecl] -> FM ([ModDecl], Global)
resolve ds =
  flip runStateT (emptyGlobal ds)
  . flip runReaderT emptyEnv
  . mapM resolveModDecl
  $ ds

resolveModDecl :: ModDecl -> ResolveM ModDecl
resolveModDecl (BindModule name expr) = do
  path <- asks curMod
  let path' = name : path
  expr' <- local (\env -> env { curMod = path' }) $ resolveModExpr expr
  modify $ \glob -> glob { mods = Map.insert path' expr' $ mods glob }
  return expr'
-- TODO

resolveModExpr :: ModExpr -> ResolveM ModExpr
resolveModExpr = undefined
-}

