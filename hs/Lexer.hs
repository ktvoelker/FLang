
module Lexer where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Common
import Token

tokenize :: String -> String -> [(Token, SourcePos)]
tokenize name xs = case parse file name xs of
  Left err -> error . show $ err
  Right ts -> ts

withPos parser = do
  pos <- getPosition
  ret <- parser
  return (ret, pos)

file :: Parser [(Token, SourcePos)]
file = do
  skippable
  xs <- many1 (withPos tok) `sepEndBy` skippable
  eof
  return . concat $ xs

skippable = many $ comment <|> (space >> return ())

tok = try keywords <|> choice [ident, exprOp, litInt, litFloat, litString, litChar]

keywords =
  choice
  . map ((>>= return . TKeyword) . string)
  $ [ "type", "val", "data", "sig", "(", ")", "open", "closed", "except", "only", "is"
    , "rec", "let", "fn", "case", "begin", "do", "?", "where", "end", "in", "of"
    , "module", "with", ":", "<:", ":>", "->", "<-", ";", "*", ".", "forall", "infix"
    , "left", "right"
    ]

ident = do
  h <- letter <|> char '_'
  t <- many $ alphaNum <|> char '_'
  return . TId . (:) h $ t

exprOp = many1 (oneOf "+-*/=<>") >>= return . TExprOp

litInt = many1 digit >>= return . TInt . read

litFloat :: Parser Token
litFloat = do
  (intPart, fracPart) <- withIntPart <|> withoutIntPart
  exp <- optionMaybe $ do
    oneOf "eE"
    sign <- optionMaybe $ oneOf "+-"
    let signVal = if sign == Just '-' then -1 else 1
    digs <- many1 digit
    return $ (signVal * read digs :: Integer)
  let intVal = read intPart :: Integer
  let fracVal = (read fracPart :: Integer) % (10 ^ length fracPart)
  return . TFloat $ (fromInteger intVal + fracVal) * (10 ^ maybe 0 id exp)
  where
    withIntPart = do
      intPart <- many1 digit
      char '.'
      fracPart <- many digit
      return (intPart, fracPart)
    withoutIntPart = do
      char '.'
      fracPart <- many1 digit
      return ("0", fracPart)

-- TODO better comment syntax, with nestable block comments
comment :: Parser ()
comment = do
  string "//"
  many . noneOf $ "\n\r"
  optional . char $ '\r'
  (char '\n' >> return ()) <|> eof

escapeCodes =
  map (\(e, r) -> (char e :: Parser Char) >> return r)
    [ ('b', '\b'), ('t', '\t'), ('n', '\n'), ('f', '\f'), ('r', '\r'), ('"', '"')
    , ('\'', '\''), ('\\', '\\')
    ]

charContent quote = (char '\\' >> escape) <|> normal
  where
    escape = foldr (<|>) (unicode <|> octal) escapeCodes
    unicode = do
      char 'u' :: Parser Char
      count 4 hexDigit >>= return . chr . read . ("0x" ++)
    octal = do
      a <- oneOf ['0' .. '3']
      [b, c] <- count 2 . oneOf $ ['0' .. '7']
      return . chr $ (readDigit a * (8 ^ 2)) + (readDigit b * 8) + readDigit c
    normal = noneOf [quote, '\\']
    readDigit = read . (: [])

litString = let q = char '"' in fmap TString . between q q . many . charContent $ '"'

litChar = let q = char '\'' in fmap TChar . between q q . charContent $ '\''

