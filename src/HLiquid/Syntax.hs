module HLiquid.Syntax where
  import Data.Text

  data Liquid 
    = HTML Text 
    | ReturnBlock Expression 
    | Block Expression

  data Expression = Expression String -- Temporary Expand to actual expressions later

