
module Main where

import Common
import Common.IO
import Lexer
import Parser
import Renamer
import Syntax

import Text.Show.Pretty

runPhase :: (Show o) => (i -> FM o) -> i -> String
runPhase f xs = ppShow out ++ ppShow errs
  where
    (out, errs) = runFM . f $ xs

lexPhase = mapM (uncurry tokenize)

parsePhase = lexPhase' >=> mapM (uncurry parse)
  where
    lexPhase' xs = fmap (zip $ map fst xs) . lexPhase $ xs

renamePhase = parsePhase >=> rename . Record

phases =
  [ ("lex", runPhase lexPhase)
  , ("parse", runPhase parsePhase)
  , ("rename", runPhase renamePhase)
  ]

getInput xs = fmap (xs,) $ case xs of
  "-" -> getContents
  _ -> readFile xs

main = do
  (phase : files) <- getArgs
  case lookup phase phases of
    Nothing -> putStrLn $ "Unknown phase: " ++ phase
    Just fn -> mapM getInput files >>= putStrLn . fn
 
