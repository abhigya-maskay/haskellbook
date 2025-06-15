import Test.QuickCheck
import Lib
import Data.List (sort)

prop_halfIdentity :: Property
prop_halfIdentity = property $ \x -> (== x) . (*2) . half $ (x :: Float)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

prop_listOrdered :: Property
prop_listOrdered = property $ \xs -> listOrdered . sort $ (xs :: [Int])

prop_plusAssociative :: Property
prop_plusAssociative = property plusAssociative where
  plusAssociative :: Int -> Int -> Int -> Bool
  plusAssociative x y z = (x + y) + z == x + (y + z)

prop_plusCommutative :: Property
prop_plusCommutative = property plusCommutative where
  plusCommutative :: Int -> Int -> Bool
  plusCommutative x y = x + y == y + x

prop_multAssociative :: Property
prop_multAssociative = property multAssociative where
  multAssociative :: Int -> Int -> Int -> Bool
  multAssociative x y z = (x * y) * z == x * (y * z)

prop_multCommutative :: Property
prop_multCommutative = property multCommutative where
  multCommutative :: Int -> Int -> Bool
  multCommutative x y = x * y == y * x

genPositiveInts :: Gen (Int, Int)
genPositiveInts = do
  a <- chooseInt (1, 500)
  b <- chooseInt (1, 500)
  return (a,b)

prop_quotRem :: Property
prop_quotRem = forAll genPositiveInts (uncurry quotRemRel) where
  quotRemRel :: Int -> Int -> Bool
  quotRemRel x y = quot x y * y + rem x y == x

prop_divMod :: Property
prop_divMod = forAll genPositiveInts (uncurry divModRel) where
  divModRel :: Int -> Int -> Bool
  divModRel x y = div x y * y + mod x y == x

-- Not true
prop_exponentAssociative :: Property
prop_exponentAssociative = property exponentAssociative where
  exponentAssociative :: Int -> Int -> Int -> Bool
  exponentAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

-- Not true
prop_exponentCommutative :: Property
prop_exponentCommutative = property exponentCommutative where
  exponentCommutative :: Int -> Int -> Bool
  exponentCommutative x y = x ^ y == y ^ x

prop_dollar :: Property
prop_dollar = property dollar where
  dollar :: (Int -> Int) -> Int -> Bool
  dollar f a = (f $ a) == f a

main :: IO()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck prop_exponentAssociative
  quickCheck prop_exponentCommutative
