-- 1)
i :: a -> a
i x = x

-- 2)
c :: a -> b -> a
c x y = x

-- 3)
c'' :: b -> a -> b
c'' x y = x

-- 4)
c' :: a -> b -> b
c' x y = y

-- 5)
r :: [a] -> [a]
r = reverse

-- 6)
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

-- 7)
a :: (a -> c) -> a -> a
a aToC a = a

-- 8)
a' :: (a -> b) -> a -> b
a' aToB a = aToB a
