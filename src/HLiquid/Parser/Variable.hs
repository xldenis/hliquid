{-# LANGUAGE OverloadedStrings #-}
module HLiquid.Parser.Variable where

import HLiquid.Syntax

import Text.Megaparsec
import Text.Megaparsec.Text

import HLiquid.Lexer

assignTag :: Parser Statement
assignTag = try . braces $ do
  symbol "assign"
  placeHolder
  return $ Assign "" ""

incrementTag :: Parser Statement
incrementTag = try . braces $ symbol "increment" *> placeHolder *> pure Increment

decrementTag :: Parser Statement
decrementTag = try . braces $ symbol "decrement" *> placeHolder *> pure Decrement

captureTag :: Parser Statement -> Parser Statement
captureTag e = do
  try . braces $ symbol "capture" *> placeHolder
  many e
  braces $ symbol "endcapture"
  return $ Capture
