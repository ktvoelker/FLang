
module Resolver where

import Common
import Syntax
import Types

type ResolveM = ReaderT Env (StateT Global FM)

resolve :: [ModDecl] -> FM Global
resolve = execStateT (runReaderT f emptyEnv) . emptyGlobal
  where
    f = do
      r <- access gRoot >>= mapM resolveModDecl
      gRoot ~= r

resolveModDecl :: ModDecl -> ResolveM ModDecl
resolveModDecl = undefined

