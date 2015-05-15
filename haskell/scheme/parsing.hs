module Parsing where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec (
  (<|>),
  Parser,
  char,
  digit,
  letter,
  many,
  many1,
  noneOf,
  oneOf,
  parse,
  skipMany1,
  space,
  string,
  )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChar <|> noneOf "\"\\"
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseBool :: Parser LispVal
parseBool = do
  char '#'
  bool <- oneOf "tf"
  return $ case bool of
    't' -> Bool True
    'f' -> Bool False

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseBool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

innerString :: Parser [Char]
innerString = liftM (concat) $ many stringChars
-- double use of many means stringChars must not match the empty string
  where stringChars = many1 (noneOf "\"") <|> string "\\\""

escapedChar :: Parser Char
escapedChar = do
  char '\\'
  c <- oneOf "\\\"" <|> letter <|> digit
  return $ case c of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> c


-- Debug routines

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

main :: IO ()
main = interact $ (++ "\n") . readExpr
