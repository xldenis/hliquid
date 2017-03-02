{-# LANGUAGE OverloadedStrings #-}
module HLiquid.ParserSpec where

import HLiquid.Parser

import Test.Hspec

import SpecHelper

spec :: Spec
spec = parallel $ do
  filesShouldParse "test/parser/success" fullFile
  filesShouldFail  "test/parser/failure" fullFile

  describe "tag braces match" $ do
    "{%- include '' }}" `parserFails` liquid
    "{{- 'test' %}" `parserFails` liquid
    "{%- include '' %}" `parserWorks` liquid
    "{{- 'test' }}" `parserWorks` liquid
