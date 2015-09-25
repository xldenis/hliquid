module HLiquid.Syntax where
  import Data.Text

  data Liquid 
    = HTML Text 
    | ReturnBlock Expression 
    | Block Expression

  data Expression = Text -- Temporary Expand to actual expressions later

