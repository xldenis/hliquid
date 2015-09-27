module HLiquid.Parser where

import Prelude hiding (lex)
import HLiquid.Syntax
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Text

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator

lex = L.lexeme space

retBlock :: Parser Liquid
retBlock = do
  lex $ string "{%"
  exp <- expression
  lex $ string "%}"
  return $ ReturnBlock exp

block :: Parser Liquid
block = do
  lex $ string "{{"
  exp <- expression
  lex $ string "}}"
  return $ Block  exp


expression :: Parser Expression
expression = do
  body <- manyTill anyChar (try $ choice [string "}}", string "%}"])
  return $ Expression body

html = undefined