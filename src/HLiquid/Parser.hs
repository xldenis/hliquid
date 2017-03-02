{-# LANGUAGE OverloadedStrings #-}
module HLiquid.Parser where

import Text.Megaparsec
import Text.Megaparsec.Text

import Data.Text (pack)
import Data.Maybe (maybeToList)

import HLiquid.Lexer
import HLiquid.Syntax

import HLiquid.Parser.Variable
import HLiquid.Parser.Expression

fullFile = many liquid <* eof

liquid :: Parser Statement
liquid = assignTag
      <|> bodyText
      <|> breakTag
      <|> captureTag liquid
      <|> caseTag
      <|> commentTag
      <|> continueTag
      <|> cycleTag
      <|> decrementTag
      <|> formTag
      <|> forTag
      <|> ifTag
      <|> incrementTag
      <|> layoutTag
      <|> includeTag
      <|> sectionTag
      <|> paginateTag
      <|> tablerowTag
      <|> unlessTag
      <|> schemaTag
      <|> expressionTag

ifTag :: Parser Statement
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
                      return $ Branch Comment body

unlessTag :: Parser Statement
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
                      return $ Branch Comment body

caseTag :: Parser Statement
caseTag = do
  try . tag' $ symbol "case" <* placeHolder
  body <- some $ (try . tag' $ symbol "when" *> placeHolder) >> When <$> many liquid

  e <- optional . try $ do
    tag' $ symbol "else"
    b <- many liquid
    return $ Else b
  tag' $ symbol "endcase"

  return $ Case body

forTag :: Parser Statement
forTag = do
  try . tag' $ symbol "for" *> placeHolder
  b <- many $ liquid

  e <- optional . try $ do
    tag' $ symbol "else"
    b <- many liquid
    return $ Else b

  tag' $ symbol "endfor"
  return $ For Form b

tablerowTag :: Parser Statement
tablerowTag = do
  try . tag' $ symbol "tablerow"
  b <- many $ liquid
  tag' $ symbol "endtablerow"

  return $ Tablerow

breakTag :: Parser Statement
breakTag = try . tag' $ symbol "break" *> pure Break

continueTag :: Parser Statement
continueTag = try . tag' $ symbol "continue" *> pure Continue

cycleTag :: Parser Statement
cycleTag = try . tag' $ symbol "cycle" *> placeHolder *> pure (Cycle [])

layoutTag :: Parser Statement
layoutTag = try . tag' $ do
  symbol "layout"
  placeHolder
  pure Layout

includeTag :: Parser Statement
includeTag = try . tag' $ do
  symbol "include"
  placeHolder
  pure $ Include ""

sectionTag :: Parser Statement
sectionTag = try . tag' $ do
  symbol "section"
  placeHolder
  pure $ Section

formTag :: Parser Statement
formTag = do
  try . tag' $ symbol "form" *> placeHolder
  b <- many $ liquid
  tag' $ symbol "endform"

  return $ Form

paginateTag :: Parser Statement
paginateTag = do
  try . tag' $ symbol "paginate" *> placeHolder
  b <- many $ liquid
  tag' $ symbol "endpaginate"

  return $ Paginate

commentTag :: Parser Statement
commentTag = do
  try . tag' $ symbol "comment"
  many $ liquid
  tag' $ symbol "endcomment"
  return $ Comment

schemaTag :: Parser Statement
schemaTag = do
  try . tag' $ symbol "schema"
  many $ liquid
  tag' $ symbol "endschema"
  return $ Schema

expressionTag :: Parser Statement
expressionTag = tag $ do
  e <- filteredExpression
  return $ Expression e

bodyText :: Parser Statement
bodyText = LitString . pack <$> do
  notFollowedBy openB
  someTill anyChar $ lookAhead openB
  where openB = (string "{{" <|> string "{%" <|> eof *> pure "e")
