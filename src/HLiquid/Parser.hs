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
liquid = choice
  [ assignTag, bodyText, breakTag, captureTag liquid, caseTag
  , commentTag, continueTag, cycleTag, decrementTag, formTag
  , forTag, ifTag, incrementTag, layoutTag, includeTag, sectionTag
  , paginateTag, tablerowTag, unlessTag, schemaTag, rawTag, expressionTag
  ]

placeHolderTag :: Parser Statement
placeHolderTag = braces $ placeHolder *> (pure Break)

-- type TagParser a = (String, Parser (a -> Statement), (a -> Statement) -> Parser Statement)
-- can be made into just a `Parser a` --------------------^

ifTag :: Parser Statement
ifTag = do
  b1 <- branch "if"
  branches <- many $ branch "elsif"
  e <- optional . try $ do
    braces $ symbol "else"
    b <- many liquid
    return $ Else b

  simpleTag "endif" (pure ())
  return . If $ b1 : (branches ++ maybeToList e)

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

branch :: String -> Parser Branch
branch nm = do
  cond <- simpleTag nm filteredExpression
  body <- many $ liquid
  return $ Branch cond body

caseTag :: Parser Statement
caseTag = tag "case" (Case <$> filteredExpression) $ \f -> do
  body <- some $ do
    w <- simpleTag "when" (When <$> filteredExpression)
    w <$> many liquid

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
          return $ (curry f) Break b

tablerowTag :: Parser Statement
tablerowTag = simpleBlockTag "tablerow" head (many liquid)
  where head = Tablerow <$> placeHolder

breakTag :: Parser Statement
breakTag = simpleTag "break" (pure Break)

continueTag :: Parser Statement
continueTag = simpleTag "continue" (pure Continue)

cycleTag :: Parser Statement
cycleTag = simpleTag "cycle" (placeHolder *> (pure $ Cycle []))

layoutTag :: Parser Statement
layoutTag = simpleTag "layout"
  (Layout <$> filteredExpression)

includeTag :: Parser Statement
includeTag = simpleTag "include" body
  where body = do
          e <- filteredExpression
          o <- optional $ do
            symbol "with"
            choice [namedArg `sepBy1` (symbol ","), pure <$> filteredExpression]
          w <- many $ (symbol "," *> namedArg)
          return $ Include e
        namedArg = (try $ identifier *> symbol ":") *> filteredExpression

sectionTag :: Parser Statement
sectionTag = simpleTag "section"
  (Section <$> filteredExpression)

formTag :: Parser Statement
formTag = simpleBlockTag "form" head (many liquid)
  where head = Form <$> filteredExpression <*> startBy expression (symbol ",")
        startBy p sep = (sep *> sepBy p sep) <|> pure []

paginateTag :: Parser Statement
paginateTag = simpleBlockTag "paginate" head (many liquid)
  where head = Paginate <$> placeHolder

-- | Comment should not parse inner liquid
commentTag :: Parser Statement
commentTag = simpleBlockTag "comment" head (body)
  where head = pure $ Comment
        body = do
          notFollowedBy (string "{% endcomment")
          someTill anyChar $ lookAhead (string "{% endcomment")

schemaTag :: Parser Statement
schemaTag = simpleBlockTag "schema" head (many liquid)
  where head = pure $ Schema

rawTag :: Parser Statement
rawTag = simpleBlockTag "raw" head (body)
  where head = pure Raw
        body = do
          notFollowedBy (string "{%")
          someTill anyChar $ lookAhead (string "{%")

expressionTag :: Parser Statement
expressionTag = try . braces' $ do
  e <- filteredExpression
  return $ Expression e

bodyText :: Parser Statement
bodyText = LitString . pack <$> do
  notFollowedBy openB
  someTill anyChar $ lookAhead openB
  where openB = ((void $ string "{{") <|> (void $ string "{%") <|> eof)
