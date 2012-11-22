
module Lexicon where

#include "Common.h"
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Token

tokenize :: Text -> Text -> [Token]
tokenize name xs = case parse file (Text.unpack name) xs of
  Left err -> error . show $ err
  Right ts -> ts

file :: Parser [Token]
file = skippable >> many1 tok `sepEndBy` skippable >>= return . concat

skippable = many $ comment <|> spaces

tok = foldl1 (<|>) $ keywords ++ [ident, exprOp, litInt, litFloat, litString, litChar]

keywords =
  map ((>>= return . TKeyword . Text.pack) . string . Text.unpack)
    [ "type", "val", "data", "sig", "(", ")", "open", "closed", "except", "only", "is"
    , "rec", "let", "fn", "case", "begin", "do", "?", "where", "end", "in", "of"
    , "module", "with", ":", "<:", ":>", "->", "<-", ";", "*", ".", "forall", "infix"
    , "left", "right"
    ]

ident = do
  head <- letter <|> char '_'
  tail <- many $ alphaNum <|> char '_'
  return . TId . Text.cons head . Text.pack $ tail

exprOp = many1 (oneOf "+-*/=<>") >>= return . TExprOp . Text.pack

litInt = many1 digit >>= return . TInt . read . Text.pack

litFloat :: Parser Token
litFloat = do
  (intPart, fracPart) <- withIntPart <|> withoutIntPart
  exp <- optionMaybe $ do
    oneOf "eE"
    sign <- optionMaybe $ oneOf "+-"
    let signVal = if sign == Just '-' then -1 else 1
    digs <- many1 digit
    return $ (signVal * read (Text.pack digs) :: Integer)
  let intVal = read (Text.pack intPart) :: Integer
  let fracVal = (read (Text.pack fracPart) :: Integer) % (10 ^ Text.length (Text.pack fracPart))
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

comment = undefined

litString = undefined

litChar = undefined

{--

  // TODO better comment syntax, with nestable block comments
  def comment: Parser[Unit] = """//[^\n\r]*\r?\n""".r ~> success(())

  def charContent(quote: String, card: String) =
    ("""(\\[btnfr"'\\]|\\u[0-9a-fA-F]{4}|\\[0-3]?[0-7]{1,2}|[^\\""" + quote + "])" + card).r;

  def string: Parser[TString] = "\"" ~> charContent("\"", "*") <~ "\"" ^^ TString;

  def char: Parser[TChar] = "'" ~> charContent("'", "") <~ "'" ^^ TChar;

--}

