{-# LANGUAGE OverloadedStrings #-}
module HLiquid.Parser where

import Control.Monad (void)

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
          cond <- simpleTag nm placeHolder
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

  simpleTag "endunless" (pure ())
  return . If $ b1 : (branches ++ maybeToList e)

  where branch nm = do
          cond <- simpleTag nm placeHolder
          body <- many $ liquid
          return $ Branch Comment body

caseTag :: Parser Statement
caseTag = tag "case" head body
  where head = placeHolder *> (pure Case)
        body f = do
          body <- some $ do
            simpleTag "when" (void placeHolder)
            When <$> many (try liquid)

          e <- optional . try $ do
            braces $ symbol "else"
            b <- many liquid
            return $ Else b
          simpleTag "endcase" (pure ())
          return $ f body

forTag :: Parser Statement
forTag = tag "for" head body
  where head = placeHolder *> (pure . uncurry $ For)
        body f = do
          b <- many $ liquid
          e <- optional . try $ do
            braces $ symbol "else"
            b <- many liquid
            return $ Else b
          braces $ symbol "endfor"
          return $ (curry f) Form b

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
tablerowTag = simpleBlockTag "tablerow" head (many liquid)
  where head = placeHolder *> (pure . const $ Tablerow)

formTag :: Parser Statement
formTag = simpleBlockTag "form" head (many liquid)
  where head = placeHolder *> (pure . const $ Form)

paginateTag :: Parser Statement
paginateTag = simpleBlockTag "paginate" head (many liquid)
  where head = placeHolder *> (pure . const $ Paginate)

commentTag :: Parser Statement
commentTag = simpleBlockTag "comment" head (many liquid)
  where head = pure . const $ Comment

schemaTag :: Parser Statement
schemaTag = simpleBlockTag "schema" head (many liquid)
  where head = pure . const $ Schema

expressionTag :: Parser Statement
expressionTag = try . braces' $ do
  e <- filteredExpression
  return $ Expression e

bodyText :: Parser Statement
bodyText = LitString . pack <$> do
  notFollowedBy openB
  someTill anyChar $ lookAhead openB
  where openB = (string "{{" <|> string "{%" <|> eof *> pure "e")
