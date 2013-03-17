
module Lexer (tokenize) where

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import Common
import Token

tokenize :: String -> String -> FM [(Token, SourcePos)]
tokenize name xs = case parse file name xs of
  Left err -> fatal . Err ELexer Nothing Nothing . Just . show $ err
  Right ts -> return ts

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

skippable = void . optional . many1 $ comment <|> (space >> return ())

tok = keywords <|> choice [ident, exprOp, litInt, litFloat, litString, litChar]

keywords =
  choice
  . map (try . (>>= return . TKeyword) . string)
  $ [ "type", "val", "data", "sig", "(", ")", "open", "closed", "except", "only", "is"
    , "rec", "let", "fn", "case", "if", "then", "else", "{", "}", "do", "in", "module"
    , "with", ":", "<:", ":>", "<-", ";", "forall", "infix" , "left", "right", "of"
    ]

ident = do
  h <- letter <|> char '_'
  t <- many $ alphaNum <|> char '_'
  return . TId . (:) h $ t

exprOp = many1 (oneOf "+-*/=:<>.?!@#$%^&|~") >>= return . TExprOp

litInt = many1 digit >>= return . TInt . read

litFloat :: Parser Token
litFloat = do
  (intPart, fracPart) <- withIntPart <|> withoutIntPart
  exp <- optionMaybe $ do
    _ <- oneOf "eE"
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
      _ <- char '.'
      fracPart <- many digit
      return (intPart, fracPart)
    withoutIntPart = do
      _ <- char '.'
      fracPart <- many1 digit
      return ("0", fracPart)

-- TODO better comment syntax, with nestable block comments
comment :: Parser ()
comment = do
  _ <- string "//"
  _ <- many . noneOf $ "\n\r"
  void . optional . char $ '\r'
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
      _ <- char 'u' :: Parser Char
      count 4 hexDigit >>= return . chr . read . ("0x" ++)
    octal = do
      a <- oneOf ['0' .. '3']
      [b, c] <- count 2 . oneOf $ ['0' .. '7']
      return
        . chr
        $ (readDigit a * 8 ^ (2 :: Integer)) + (readDigit b * 8) + readDigit c
    normal = noneOf [quote, '\\']
    readDigit = read . (: [])

litString = let q = char '"' in fmap TString . between q q . many . charContent $ '"'

litChar = let q = char '\'' in fmap TChar . between q q . charContent $ '\''

