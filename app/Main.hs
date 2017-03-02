module Main where

import Lib
import HLiquid.Parser

import System.Environment
import Data.Either

main :: IO ()
main = do
  files <- getArgs
  results <- mapM (\file -> parseFile fullFile file) files
  let success = length (rights results)
  mapM_ (putStrLn) (lefts results)

  putStrLn $ "Parsed " ++ (show success) ++ " / " ++ (show $ length results) ++ " successfully."
