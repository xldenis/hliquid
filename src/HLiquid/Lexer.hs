module HLiquid.Lexer where

import           Control.Monad
import           Control.Applicative (empty)
import           Control.Monad       (void, join)

import           Data.Scientific

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol' sc

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

tag :: Parser a -> Parser a
tag = between (symbol "{{-" <|> symbol "{{") (symbol "-}}" <|> symbol "}}")

tag' :: Parser a -> Parser a
tag' = between (symbol "{%-" <|> symbol "{%") (symbol "-%}" <|> symbol "%}")

placeHolder :: Parser String
placeHolder = someTill anyChar (lookAhead $ string "%}")

number :: Parser Scientific
number = L.signed sc L.number

identifier :: Parser String
identifier = many $ alphaNumChar <|> oneOf "_-"

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')
