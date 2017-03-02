module HLiquid.Lexer where

import           Control.Monad
import           Control.Applicative (empty)
import           Control.Monad       (void, join, liftM)

import           Data.Scientific

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol' sc

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

data TagType
  = Return
  | ReturnTrim
  | Statement
  | StatementTrim
  deriving (Show, Eq)

chooseTagType :: Parser TagType
chooseTagType = openBraces <|> openBraces'

openBraces :: Parser TagType
openBraces = symbol "{%-" *> pure StatementTrim <|> symbol "{%" *> pure Statement

openBraces' :: Parser TagType
openBraces' = symbol "{{-" *> pure ReturnTrim <|> symbol "{{" *> pure Return

chooseOpenBraces :: TagType -> Parser TagType
chooseOpenBraces Return         = symbol "{{-" *> pure ReturnTrim <|> symbol "{{" *> pure Return
chooseOpenBraces ReturnTrim     = symbol "{{-" *> pure ReturnTrim <|> symbol "{{" *> pure Return
chooseOpenBraces _              = symbol "{%-" *> pure StatementTrim <|> symbol "{%" *> pure Statement

closeBraces :: TagType -> Parser TagType
closeBraces Return         = symbol "}}" *> pure Return <|> symbol "-}}" *> pure ReturnTrim
closeBraces ReturnTrim     = symbol "}}" *> pure Return <|> symbol "-}}" *> pure ReturnTrim
closeBraces Statement      = symbol "%}" *> pure Statement <|> symbol "-%}" *> pure StatementTrim
closeBraces StatementTrim  = symbol "%}" *> pure Statement <|> symbol "-%}" *> pure StatementTrim

tag :: String -> Parser (a -> b) -> ((a -> b) -> Parser b) -> Parser b
tag name head body = do
  ty <- try $ openBraces <* (symbol name)
  f  <- head <* closeBraces ty
  body f

tag' :: String -> Parser (a -> b) -> ((a -> b) -> Parser b) -> Parser b
tag' name head body = do
  ty <- try $ openBraces' <* (symbol name)
  f  <- head <* closeBraces ty
  body f

simpleTag :: String -> Parser b -> Parser b
simpleTag name head = tag name (liftM const head) (\x -> return $ x ())

simpleBlockTag :: String -> Parser (() -> b) -> Parser c -> Parser b
simpleBlockTag name head body = tag name (head) $ \f -> do
  body
  braces . symbol $ "end" ++ name
  return $ f ()

braces :: Parser a -> Parser a
braces a = do
  ty <- openBraces
  a <* closeBraces ty

braces' :: Parser a -> Parser a
braces' a = do
  ty <- openBraces'
  a <* closeBraces ty

placeHolder :: Parser String
placeHolder = someTill anyChar (lookAhead $ string "-%}" <|> string "%}")

number :: Parser Scientific
number = L.signed sc L.number

reserved :: [String]
reserved = ["endcase", "endif", "endcomment", "endpaginate", "endfor", "endform", "for", "assign", "if", "endcapture", "endunless", "endschema"]

identifier :: Parser String
identifier = p >>= res
  where p = label "identifier" . lexeme $ ((:) <$> identChar <*> many identChar)
        res i = if i `elem` reserved then
            fail $ "The reserved word `" ++ i ++ "` cannot be used as an identifier."
          else
            return i
        identChar = alphaNumChar <|> oneOf "_-"

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')
