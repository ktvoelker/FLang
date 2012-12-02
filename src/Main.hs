
module Main where

import Common.IO
import Lexer
import Parser

import Text.Show.Pretty

lexPhase file = show . tokenize file

parsePhase file = ppShow . parse file

phases =
  [ ("lex", lexPhase)
  , ("parse", parsePhase)
  ]

main = do
  [phase, file] <- getArgs
  case lookup phase phases of
    Nothing -> putStrLn $ "Unknown phase: " ++ phase
    Just fn ->
      (if file == "-" then getContents else readFile file) >>= putStrLn . fn file
 
