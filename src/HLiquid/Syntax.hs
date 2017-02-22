module HLiquid.Syntax where
import Data.Text

data Liquid
  = HTML Text
  | ReturnBlock Expression
  | Block Expression
  deriving (Eq, Show)

data When
  = When [Statement]
  deriving (Eq, Show)

data Branch
  = Branch Statement [Statement]
  | Else [Statement]
  deriving (Eq, Show)

data Statement
  = Expression Expression -- Temporary Expand to actual expressions later
  | If [Branch] -- unless and elseif
  | Case [When]
  -- Loop Tags
  | For Statement [Statement]
  | Break
  | Continue
  | Cycle [Statement]
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
  | Increment
  | Decrement
  -- | Variable Text
  | Handle Statement Text
  -- Filters
  -- | Filter
  -- Types
  | LitString Text

  -- Shopify :(
  -- | Form
  deriving (Eq, Show)

data Operator
  = Equal
  | NotEq
  | Greater
  | Less
  | GreaterEq
  | LessEq
  | Or
  | And
  | Contains
  deriving (Eq, Show)

data Expression
  = String String
  | Integer Integer
  | Float Double
  | Boolean Bool
  | Nil
  | Array Expression
  | Op Operator Expression Expression
  | Filter Expression  String [Expression]
  | Variable [String]
  deriving (Eq, Show)
