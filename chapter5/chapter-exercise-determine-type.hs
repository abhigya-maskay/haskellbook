{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where
-- 1)
-- a)
a :: Num a => a
a = (* 9) 6 -- Returns 54

-- b)
b :: (Num a) => (a, String)
b = head [(0, "doge"), (1, "kitteh")]

-- c)
c :: (Integer, String)
c = head [(0 :: Integer, "doge"), (1, "kitteh")]

-- d)
d :: Bool
d = if False then True else False

-- e)
e :: Int
e = length [1,2,3,4,5]

-- f)
f :: Bool
f = length [1,2,3,4] > length "TACOCAT"

-- 2)
x = 5
y = x + 5
w :: Num a => a
w = y * 10

-- 3)
z :: Num a => a -> a
z y = y * 10

-- 4)
f' :: Fractional a => a
f' = 4 / y

-- 5)
x' = "Julie"
y' = "<3"
z' = "Haskell"
f'' :: String
f'' = x' ++ y' ++ z'
