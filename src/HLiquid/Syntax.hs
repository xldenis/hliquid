module HLiquid.Syntax where
import Data.Text

data Liquid
  = HTML Text
  | ReturnBlock Expression
  | Block Expression
  deriving (Eq, Show)

data When
  = When Expression [Statement]
  deriving (Eq, Show)

data Branch
  = Branch Expression [Statement]
  | Else [Statement]
  deriving (Eq, Show)

data Statement
  = Expression Expression -- Temporary Expand to actual expressions later
  | If [Branch] -- unless and elseif
  | Case Expression [When]
  -- Loop Tags
  | For Statement [Statement]
  | Break
  | Continue
  | Cycle [Statement]
  | Tablerow String [Statement]
  -- Layout Tag
  | Comment String
  | Include Expression
  | Form Expression [Expression] [Statement]
  | Layout Expression
  | Paginate String [Statement]
  | Raw String
  | Section Expression
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
  | Schema [Statement]
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
  | Variable String
  | Selector [Expression]
  | Index Expression Expression
  deriving (Eq, Show)

