module TestUtil where

import Data.Attoparsec.Text
import Data.Either (isLeft)
import qualified Data.Text as T
import Test.Hspec

parseResultShouldBe :: (Eq a, Show a) => Parser a -> T.Text -> a -> Expectation
parseResultShouldBe parser text expected = case parseOnly parser text of
  Right actual -> expected `shouldBe` actual
  Left err -> expectationFailure err

parseResultShouldError :: (Show a) => Parser a -> T.Text -> Expectation
parseResultShouldError parser text = parseOnly parser text `shouldSatisfy` isLeft
