module HLiquid.Syntax where
  import Data.Text

  data Liquid 
    = HTML Text 
    | If Expression Expression Expression
    | ReturnBlock Expression 
    | Block Expression
    deriving (Eq, Show)

  data Expression = Expression [Text] -- Temporary Expand to actual expressions later
    deriving (Eq, Show)
