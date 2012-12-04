
module Resolver where

import Common
import Syntax
import Types

type ResolveM = ReaderT Env (StateT Global FM)

resolve :: [ModDecl] -> FM Global
resolve = execStateT (runReaderT f emptyEnv) . emptyGlobal
  where
    f = gRoot %>>= mapM resolveModDecl

resolveModDecl :: ModDecl -> ResolveM ModDecl
resolveModDecl = undefined

