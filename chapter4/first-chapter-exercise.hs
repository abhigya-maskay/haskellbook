awesome = ["Papuchon", "curry", ":)"]

also = ["Quake", "The Simons"]

allAwesome = [awesome, also]

-- 1.
length' :: [a] -> Int
length' = undefined

-- 2
a = length [1, 2, 3, 4, 5] -- 5

b = length [(1, 2), (2, 3), (3, 4)] -- 3

c = length allAwesome -- 2

d = length (concat allAwesome) -- 4

-- 3
-- The second expression will return an error because the numbers are treated as
-- polymorphic and a value of type Num while the length function returns a value of the
-- specific type Int which is not fractional

{-4.
The code from the previous exercise could be fixed by using integral division (div) instead
of the operator that attempts a fractional division
-}

{-5.
The type is Bool (The result is true)
-}

{-
6.
a. This is a declaration. X is of type Num a => a
b. This is a statement of type Bool
-}

-- 7.
a' = length allAwesome == 2

{- b' = length [1, 'a', 3, 'b']
This one wont work because the list cannot have elements of different types -}
c' = length allAwesome + length awesome

d' = (8 == 8) && ('b' < 'a')

{- e' = (8 == 8) && 9
This one wont work because the second value (9) is not of type Bool -}

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.
myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t t' = ((snd t, snd t'), (fst t, fst t'))
