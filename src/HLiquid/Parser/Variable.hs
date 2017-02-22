{-# LANGUAGE OverloadedStrings #-}
module HLiquid.Parser.Variable where

import HLiquid.Syntax

import Text.Megaparsec
import Text.Megaparsec.Text

import HLiquid.Lexer

assignTag :: Parser Statement
assignTag = try . tag' $ do
  symbol "assign"
  placeHolder
  return $ Assign "" ""

incrementTag :: Parser Statement
incrementTag = try . tag' $ symbol "increment" *> placeHolder *> pure Increment

decrementTag :: Parser Statement
decrementTag = try . tag' $ symbol "decrement" *> placeHolder *> pure Decrement

captureTag :: Parser Statement -> Parser Statement
captureTag e = do
  try . tag' $ symbol "capture" *> placeHolder
  many e
  tag' $ symbol "endcapture"
  return $ Capture
