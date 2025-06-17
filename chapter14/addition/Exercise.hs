module Exercise where

import Test.Hspec


recursiveMult :: (Eq a, Num a) => a -> a -> a
recursiveMult x 0 = 0
recursiveMult x y = x + (recursiveMult x (y - 1))

main :: IO()
main = hspec $ do
  describe "Multiplication" $ do
    it "5 multiplied by 0" $ do
      recursiveMult 5 0 `shouldBe` 0
    it "5 multiplied by 3" $ do
      recursiveMult 5 3 `shouldBe` 15
