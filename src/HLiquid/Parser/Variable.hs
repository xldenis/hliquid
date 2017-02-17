{-# LANGUAGE OverloadedStrings #-}
module HLiquid.Parser.Variable where

import HLiquid.Syntax

import Text.Megaparsec
import Text.Megaparsec.Text

import HLiquid.Lexer

assignTag :: Parser Expression
assignTag = try . tag' $ do
  symbol "assign"
  placeHolder
  return $ Assign "" ""

incrementTag :: Parser Expression
incrementTag = try . tag' $ symbol "increment" *> placeHolder *> pure Increment

decrementTag :: Parser Expression
decrementTag = try . tag' $ symbol "decrement" *> placeHolder *> pure Decrement

captureTag :: Parser Expression -> Parser Expression
captureTag e = do
  try . tag' $ symbol "capture" *> placeHolder
  many e
  tag' $ symbol "endcapture"
  return $ Capture
