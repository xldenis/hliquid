module Lib where


import HLiquid.Syntax
import HLiquid.Parser

import Prelude hiding (readFile)

import Data.Text.IO (readFile)

import Text.Megaparsec
import Text.Megaparsec.Text

parseFile :: Show a => Parser a -> String -> IO (Either String a)
parseFile p str = do
  f <- readFile str
  let res = parse p str f
  case res of
    Left err -> return . Left $ parseErrorPretty err
    Right a  -> return $ Right a -- putStrLn "passed"
