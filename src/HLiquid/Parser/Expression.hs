module HLiquid.Parser.Expression where

import Control.Monad (join)

import Data.Maybe

import Data.Scientific

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Expr hiding (Operator)
import qualified Text.Megaparsec.Expr as E (Operator)

import HLiquid.Lexer
import HLiquid.Syntax

dquotes = between (char '"') (char '"')
squotes = between (char '\'') (char '\'')

stringLit :: Parser Expression
stringLit = fmap String $
          (dquotes . many $ noneOf "\"")
      <|> (squotes .  many $ noneOf "\'")

numberLit :: Parser Expression
numberLit = do
  n <- number
  return $ case floatingOrInteger n of
    Left f  -> Float f
    Right i -> Integer i

boolean :: Parser Expression
boolean = fmap Boolean $ string "true" *> pure True <|> string "false" *> pure False

nil :: Parser Expression
nil = string "nil" *> pure Nil

variable :: Parser Expression
variable = Variable <$> (identifier `sepBy` char '.')

primExpr :: Parser Expression
primExpr = lexeme $ stringLit <|> numberLit <|> boolean <|> nil <|> variable

array :: Parser Expression
array = between (char '[') (char ']') expression

expression :: Parser Expression
expression = makeExprParser primExpr opTable

filteredExpression :: Parser Expression
filteredExpression = do
  base <- expression
  filters <- many filterOp
  return $ foldr ($) base filters

filterOp = do
    try $ symbol "|"
    i <- identifier
    args <- optional $ do
      symbol ":"
      (expression) `sepBy` (symbol ",")
    sc
    return $ \a -> Filter a i (join $ maybeToList args)

opTable = [ [ binary "==" Equal
            , binary "!=" NotEq
            , binary "<"  Less
            , binary ">"  Greater
            , binary ">=" GreaterEq
            , binary "<=" LessEq
            , binary "or" Or
            , binary "and" And
            , binary "contains" Contains
            ]
          ]

binary :: String -> Operator -> E.Operator Parser Expression
binary op c = InfixL $ do
  symbol op *> (return $ \a b -> Op c a b)
