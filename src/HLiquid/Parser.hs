{-# LANGUAGE OverloadedStrings #-}
module HLiquid.Parser where

import Prelude hiding (lex)

import qualified Data.Text as T

import HLiquid.Syntax

import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Char

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

markup :: Parser Liquid
markup = do
  text <- T.pack <$> manyTill anyChar blockOpen
  return $ HTML text

blockOpen :: Parser ()
blockOpen = (try $ string "{{") <|> string "{%" >> return ()

blockClose :: Parser ()
blockClose = (try $ string "}}") <|> (try $ string "%}") >> return ()

expression :: Parser Expression
expression = do
  body <- manyTill word (lookAhead blockClose)
  return $ Expression body
  where word = T.pack <$> (lex . many $ noneOf "%} ")

html = undefined