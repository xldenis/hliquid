module HLiquid.Lexer where

import Control.Monad

import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text
import Text.Megaparsec

import Control.Applicative (empty)
import Control.Monad (void, join)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol' sc

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

tag :: Parser a -> Parser a
tag = between (symbol "{{") (symbol "}}")

tag' :: Parser a -> Parser a
tag' = between (symbol "{%") (symbol "%}")

placeHolder :: Parser String
placeHolder = someTill anyChar (lookAhead $ string "%}")
