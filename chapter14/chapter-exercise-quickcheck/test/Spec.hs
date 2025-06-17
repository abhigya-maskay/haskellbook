import Test.QuickCheck
import Lib (half, capitalizeWord)
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

instance Show (a -> b) where
  show _ = "f"

prop_dollar :: Property
prop_dollar = property dollar where
  dollar :: (Int -> Int) -> Int -> Bool
  dollar f a = (f $ a) == f a

-- Not equal
prop_foldrCons :: Property
prop_foldrCons = property foldrCons where
  foldrCons :: [Int] -> [Int] -> Bool
  foldrCons xs xs' = foldr (:) xs xs' == (xs ++ xs')

prop_foldrConcat :: Property
prop_foldrConcat = property foldrConcat where
  foldrConcat :: [[Int]] -> Bool
  foldrConcat xs = foldr (++) [] xs == concat xs

-- Not True for empty list
prop_takeLength :: Property
prop_takeLength = property takeLength where
  takeLength :: Int -> [Int] -> Bool
  takeLength n xs = length (take n xs) == n

prop_roundTrip :: Property
prop_roundTrip = property roundTrip where
  roundTrip :: Int -> Bool
  roundTrip x = (read . show $ x) == x

-- Fails on negative numbers and due to the lack of precision
prop_square :: Property
prop_square = property squareIdentity where
  square x = x * x
  squareIdentity :: Float -> Bool
  squareIdentity x = (square . sqrt $ x) == x

-- Idempotence
twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

prop_capitalizeWord :: Property
prop_capitalizeWord = property capitalizeIdem where
  capitalizeIdem :: String -> Bool
  capitalizeIdem x = (capitalizeWord x == twice capitalizeWord x) &&
    (capitalizeWord x == fourTimes capitalizeWord x)

prop_sort :: Property
prop_sort = property sortIdem where
  sortIdem :: [Int] -> Bool
  sortIdem xs = (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)

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
  quickCheck prop_dollar
  quickCheck prop_foldrCons
  quickCheck prop_foldrConcat
  quickCheck prop_takeLength
  quickCheck prop_roundTrip
  quickCheck prop_square
  quickCheck prop_capitalizeWord
  quickCheck prop_sort
