module Lib where


import HLiquid.Syntax
import HLiquid.Parser

import Prelude hiding (readFile)

import Data.Text.IO (readFile)

import Text.Megaparsec
import Text.Megaparsec.Text

parseFile :: Show a => Parser a -> String -> IO ()
parseFile p str = do
  f <- readFile str
  let res = parse p str f
  case res of
    Left err -> putStr . parseErrorPretty $ err
    Right _  -> return () -- putStrLn "passed"
