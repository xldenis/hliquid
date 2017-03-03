{-# LANGUAGE OverloadedStrings #-}
module HLiquid.ParserSpec where

import HLiquid.Parser

import Test.Hspec

import SpecHelper

spec :: Spec
spec = parallel $ do
  filesShouldParse "test/parser/liquid/success" fullFile
  filesShouldFail  "test/parser/liquid/failure" fullFile
  filesShouldParse "assets" fullFile

  describe "tag braces match" $ do
    "{%- include '' }}" `parserFails` liquid
    "{{- 'test' %}" `parserFails` liquid
    "{%- include '' %}" `parserWorks` liquid
    "{{- 'test' }}" `parserWorks` liquid
