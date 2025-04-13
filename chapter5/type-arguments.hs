-- 1)
f :: a -> a -> a -> a
f = undefined

x :: Char
x = undefined

-- fx is Char -> Char -> Char

-- 2)
g :: a -> b -> c -> b
g = undefined

-- g 0 'c' "woot" is Char

-- 3)
h :: (Num a, Num b) => a -> b -> b
h = undefined

-- h 1.0 2 is Num b => b

-- 4)
-- h 1 (5.5 :: Double) Double

-- 5)
jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined
-- jackal "keyboard" "has the word jackal in it" is [Char]

-- 6)
-- jackal "keyboard" is Eq b => b -> [Char]

-- 7)
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined
-- kessel 1 2  is (Ord a, Num a) => a

-- 8)
-- kessel 1 (2::Integer) is (Ord a, Num a) a

-- 9)
 -- kessel (1 :: Integer) 2 is Integer
