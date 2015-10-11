module HLiquid.Syntax where
  import Data.Text

  data Liquid 
    = HTML Text 
    | ReturnBlock Expression 
    | Block Expression
    deriving (Eq, Show)

  data Expression 
    = Expression [Text] -- Temporary Expand to actual expressions later
    | If -- unless and elseif
    | Case
    -- Loop Tags
    | For
    | Break
    | Continue
    | Cycle
    | Tablerow
    -- Layout Tag
    | Comment
    | Include
    | Form
    | Layout
    | Paginate
    | Raw
    -- Variable Tag
    | Assign
    | Capture
    | Increment
    | Decrement
    -- Filters
    | Filter
    deriving (Eq, Show)


  data Operator 
    = Equal
    | NotEqual
    | Greater
    | Less
    | GreaterEq
    | LessEq
    | Or
    | And