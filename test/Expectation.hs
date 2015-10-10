module Expectation where
  import Test.Hspec.Expectations
  
  import Text.Megaparsec
  import Text.Megaparsec.Text

  import Data.Text

  import Control.Monad (unless)
  
  parses :: Parser a -> Text -> Either ParseError a
  parses par str = parse par "" str

  (~>) :: (Show a, Eq a, Show b) => Either b a -> a -> Expectation 
  res ~> want = case res of
      Right a -> unless (a == want) $ expectationFailure $ "got "++show a++" expected "++show want
      Left b -> expectationFailure $ show b

  success :: Either a b -> (b -> Expectation) -> Expectation
  success res f = case res of
      Right c -> f c
      Left _ -> expectationFailure $ "operation failed"

  wontParse :: Parser a -> Text -> Expectation
  wontParse par str = case parse par "" str of
    Right _ -> expectationFailure $ "input `"++ (unpack str) ++ "` shouldnt have parsed."
    _ -> return ()