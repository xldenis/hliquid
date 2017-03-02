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
    braces $ symbol "else"
    b <- many liquid
    return $ Else b

  braces $ symbol "endif"
  return . If $ b1 : (branches ++ maybeToList e)

  where branch nm = do
                      cond <- try . braces $ symbol nm *> placeHolder
                      body <- many $ liquid
                      return $ Branch Comment body

unlessTag :: Parser Statement
unlessTag = do
  b1 <- branch "unless"

  branches <- many $ branch "elsif"

  e <- optional . try $ do
    braces $ symbol "else"
    b <- many liquid
    return $ Else b

  braces $ symbol "endunless"
  return . If $ b1 : (branches ++ maybeToList e)

  where branch nm = do
                      cond <- try . braces $ symbol nm *> placeHolder
                      body <- many $ liquid
                      return $ Branch Comment body

caseTag :: Parser Statement
caseTag = do
  try . braces $ symbol "case" <* placeHolder
  body <- some $ do
    try . braces $ symbol "when" *> placeHolder
    When <$> many (try liquid)

  e <- optional . try $ do
    braces $ symbol "else"
    b <- many liquid
    return $ Else b
  braces $ symbol "endcase"

  return $ Case body

forTag :: Parser Statement
forTag = do
  try . braces $ symbol "for" *> placeHolder
  b <- many $ liquid

  e <- optional . try $ do
    braces $ symbol "else"
    b <- many liquid
    return $ Else b

  braces $ symbol "endfor"
  return $ For Form b

breakTag :: Parser Statement
breakTag = simpleTag "break" (pure Break)

continueTag :: Parser Statement
continueTag = simpleTag "continue" (pure Continue)

cycleTag :: Parser Statement
cycleTag = try . braces $ symbol "cycle" *> placeHolder *> pure (Cycle [])

layoutTag :: Parser Statement
layoutTag = simpleTag "layout"
  (placeHolder *>  (pure $ Layout))

includeTag :: Parser Statement
includeTag = simpleTag "include"
  (placeHolder *>  (pure $ Include ""))

sectionTag :: Parser Statement
sectionTag = simpleTag "section"
  (placeHolder *>  (pure $ Section ))

tablerowTag :: Parser Statement
tablerowTag = tag "tablerow" head body
  where head = placeHolder *> (pure . const $ Tablerow)
        body f = do
          b <- many $ liquid
          braces $ symbol "endtablerow"
          return $ f ()

formTag :: Parser Statement
formTag = tag "form" head body
  where head = placeHolder *> (pure . const $ Form)
        body f = do
          b <- many $ liquid
          braces $ symbol "endform"
          return $ f ()

paginateTag :: Parser Statement
paginateTag = tag "paginate" head body
  where head = placeHolder *> (pure . const $ Paginate)
        body f = do
          many $ liquid
          braces $ symbol "endpaginate"
          return $ f ()

commentTag :: Parser Statement
commentTag = tag "comment" head body
  where head = pure . const $ Comment
        body f = do
          many $ liquid
          braces $ symbol "endcomment"
          return $ f ()

schemaTag :: Parser Statement
schemaTag = tag "schema" head body
  where head = pure . const $ Schema
        body f = do
          many $ liquid
          braces $ symbol "endschema"
          return $ f ()

expressionTag :: Parser Statement
expressionTag = try . braces' $ do
  e <- filteredExpression
  return $ Expression e

bodyText :: Parser Statement
bodyText = LitString . pack <$> do
  notFollowedBy openB
  someTill anyChar $ lookAhead openB
  where openB = (string "{{" <|> string "{%" <|> eof *> pure "e")
