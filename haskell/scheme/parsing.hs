module Main where
import qualified Text.ParserCombinators.Parsec as Parsec

symbol :: Parsec.Parser Char
symbol = Parsec.oneOf "!#$%&|*+-/:<=>?@^_~"
