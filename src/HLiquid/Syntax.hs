module HLiquid.Syntax where
import Data.Text

data Liquid
  = HTML Text
  | ReturnBlock Expression
  | Block Expression
  deriving (Eq, Show)

data When
  = When [Expression]
  deriving (Eq, Show)
data Branch
  = Branch Expression [Expression]
  | Else [Expression]
  deriving (Eq, Show)

data Expression
  = Expression [Text] -- Temporary Expand to actual expressions later
  | If [Branch] -- unless and elseif
  | Case [When]
  -- Loop Tags
  | For Expression [Expression]
  | Break
  | Continue
  | Cycle [Expression]
  | Tablerow
  -- Layout Tag
  | Comment
  | Include Text
  | Form
  | Layout
  | Paginate
  | Raw
  -- Variable Tag
  | Assign Text Text
  | Capture -- TODO
  | Increment Expression
  | Decrement Expression
  | Variable Text
  | Handle Expression Text
  -- Filters
  | Filter
  -- Types
  | LitString Text

  -- Shopify :(
  -- | Form
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
