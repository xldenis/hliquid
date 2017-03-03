{-# LANGUAGE OverloadedStrings #-}
module HLiquid.Parser.ExpressionSpec where

import Text.Megaparsec (many, eof)

import HLiquid.Parser.Expression

import Test.Hspec

import SpecHelper

spec :: Spec
spec = do
  filesShouldParse "test/parser/expression/success/" (many expression <* eof)
  filesShouldParse "test/parser/filtered_expression/success/" (many filteredExpression <* eof)

  describe "operators" $ do
    "1 <= 2" `parserWorks` expression

  describe "filteredExpressions" $ do
    "a | filter" `parserWorks` expression

  describe "brackets" $ do
    "a[a].aaalksjafklj[b[1]]" `parserWorks` expression

  describe "identifier" $ do
    "aa?aaa" `parserFails` expression

    "aaaa?" `parserWorks` expression
