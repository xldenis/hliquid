module HLiquid.ParserSpec (spec) where

  import Test.Hspec

  import HLiquid.Parser
  import HLiquid.Syntax
  import Expectation

  spec :: Spec
  spec = do
    describe "returnBlock" $ do
      it "parses start and end" $ do
        pending
      it "leaves whitespace on outside" $ do
        pending