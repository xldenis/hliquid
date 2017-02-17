module Main where

import Lib
import HLiquid.Parser

import System.Environment

main :: IO ()
main = do
  files <- getArgs
  mapM_ (\file -> parseFile fullFile file) files
