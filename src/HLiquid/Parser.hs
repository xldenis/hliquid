{-# LANGUAGE OverloadedStrings #-}
module HLiquid.Parser where

import Text.Megaparsec
import Text.Megaparsec.Text

import Data.Text (pack)
import Data.Maybe (maybeToList)

import HLiquid.Lexer
import HLiquid.Syntax

import HLiquid.Parser.Variable

fullFile = many liquid <* eof

liquid :: Parser Expression
liquid = ifTag
      <|> unlessTag
      <|> caseTag
      <|> formTag
      <|> forTag
      <|> tablerowTag
      <|> breakTag
      <|> continueTag
      <|> cycleTag
      <|> layoutTag
      <|> commentTag
      <|> expressionTag
      <|> assignTag
      <|> captureTag liquid
      <|> paginateTag
      <|> bodyText

ifTag :: Parser Expression
ifTag = do
  b1 <- branch "if"

  branches <- many $ branch "elsif"

  e <- optional . try $ do
    tag' $ symbol "else"
    b <- many liquid
    return $ Else b

  tag' $ symbol "endif"
  return . If $ b1 : (branches ++ maybeToList e)

  where branch nm = do
                      cond <- try . tag' $ symbol nm *> placeHolder
                      body <- many $ liquid
                      return $ Branch Filter body

unlessTag :: Parser Expression
unlessTag = do
  b1 <- branch "unless"

  branches <- many $ branch "elsif"

  e <- optional . try $ do
    tag' $ symbol "else"
    b <- many liquid
    return $ Else b

  tag' $ symbol "endunless"
  return . If $ b1 : (branches ++ maybeToList e)

  where branch nm = do
                      cond <- try . tag' $ symbol nm *> placeHolder
                      body <- many $ liquid
                      return $ Branch Filter body

caseTag :: Parser Expression
caseTag = try $ do
  tag' $ symbol "case"
  body <- some $ (tag' $ symbol "when") >> When <$> many liquid
  tag' $ symbol "endcase"

  return $ Case body

forTag :: Parser Expression
forTag = do
  try . tag' $ symbol "for" *> placeHolder
  b <- many $ liquid
  tag' $ symbol "endfor"
  return $ For Form b

tablerowTag :: Parser Expression
tablerowTag = try $ do
  tag' $ symbol "tablerow"
  b <- many $ liquid
  tag' $ symbol "endtablerow"

  return $ Tablerow

breakTag :: Parser Expression
breakTag = try . tag' $ symbol "break" *> pure Break

continueTag :: Parser Expression
continueTag = try . tag' $ symbol "continue" *> pure Continue

cycleTag :: Parser Expression
cycleTag = try . tag' $ symbol "cycle" *> placeHolder *> pure (Cycle [])

layoutTag :: Parser Expression
layoutTag = try . tag' $ do
  symbol "layout"
  placeHolder
  pure Layout

formTag :: Parser Expression
formTag = do
  try . tag' $ symbol "form" *> placeHolder
  b <- many $ liquid
  tag' $ symbol "endform"

  return $ Form

paginateTag :: Parser Expression
paginateTag = do
  try . tag' $ symbol "paginate" *> placeHolder
  b <- many $ liquid
  tag' $ symbol "endpaginate"

  return $ Paginate

commentTag :: Parser Expression
commentTag = do
  try . tag' $ symbol "comment"
  many $ liquid
  try . tag' $ symbol "endcomment"
  return $ Comment

expressionTag :: Parser Expression
expressionTag = try . tag $ do
  many $ noneOf ['}']
  return $ Expression []

bodyText :: Parser Expression
bodyText = LitString . pack <$> do
  notFollowedBy openB
  someTill anyChar $ lookAhead openB
  where openB = (string "{{" <|> string "{%" <|> eof *> pure "e")
