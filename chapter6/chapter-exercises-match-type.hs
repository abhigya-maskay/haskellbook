import Data.List (sort)

i :: (Num a) => a
-- 1)
-- i :: a
-- This would not typecheck because we need an instance of Num for numeric literals
i = 1

f :: Float
-- 2)
-- f :: Num a => a
-- This would not typecheck because we need an instance of fractional for decimals
-- 3)
-- f :: Fractional a => a
-- This would typecheck because it has the required instance of fractional
-- 4)
-- f :: RealFrac a => a
-- This would typecheck because requiring RealFrac implies requiring Frac
f = 1.0

freud :: a -> a
-- 5)
-- freud :: Ord a => a -> a
-- This would typecheck because the following function does not require any constraints
-- 6)
-- freud :: Int -> Int
-- This would typecheck because it just replaces the a with a concrete type of Int
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
-- 7)
-- sigmund :: a -> a
-- Would not typecheck because myX already has the concrete type of Int
-- 8)
-- sigmund :: Num a => a -> a
-- Would not typecheck because Num a is still less concrete than Int
sigmund x = myX

-- jung :: Ord a => [a] -> a
-- 9)
-- jung :: [Int] -> Int
-- This will typecheck because Int satisfies the Ord constraint and is the same type otherwise
jung xs = head (sort xs)

young :: [Char] -> Char
-- 10)
-- young :: Ord a => [a] -> a
-- This will typecheck because Char satisfies the Ord constraint and the function is the same type otherwise
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

-- signifier :: [Char] -> Char
-- 11)
-- signifier :: Ord a => [a] -> a
-- This will not typecheck because the mySort function already has a concrete type of Char which is not satisfied by the above types
signifier xs = head (mySort xs)
