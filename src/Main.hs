
module Main where

import Common
import Common.IO
import Lexer
import Parser
import Pretty
import Renamer
import Syntax

import Text.Show.Pretty

runPhase :: (MonadIO m) => (i -> FM String) -> i -> m ()
runPhase f xs = liftIO . mapM_ putStrLn $ maybeToList out ++ map ppShow errs
  where
    (out, errs) = runFM . f $ xs

lexPhase = mapM (uncurry tokenize)

parsePhase = lexPhase' >=> mapM (uncurry parse)
  where
    lexPhase' xs = fmap (zip $ map fst xs) . lexPhase $ xs

renamePhase = parsePhase >=> rename . Record

phases =
  [ ("lex", runPhase $ lexPhase >=> return . concatMap ppShow)
  , ("parse", runPhase $ parsePhase >=> return . concatMap pretty)
  , ("rename", runPhase $ renamePhase >=> return . pretty)
  ]

getInput xs = fmap (xs,) $ case xs of
  "-" -> getContents
  _ -> readFile xs

main = do
  (phase : files) <- getArgs
  case lookup phase phases of
    Nothing -> putStrLn $ "Unknown phase: " ++ phase
    Just fn -> mapM getInput files >>= fn
 
