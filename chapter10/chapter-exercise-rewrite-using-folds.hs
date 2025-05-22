-- [[file:chapter-exercises.org::*1)][1):1]]
myOr :: [Bool] -> Bool
myOr = foldr (||) False
-- 1):1 ends here

-- [[file:chapter-exercises.org::*2)][2):1]]
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (||) False . map f
-- 2):1 ends here

-- [[file:chapter-exercises.org::*3)][3):1]]
myElem :: Eq a => a -> [a] -> Bool
myElem elem = foldr (\x y -> (x == elem) || y) False

myElem' elem = any (== elem)
-- 3):1 ends here

-- [[file:chapter-exercises.org::*4)][4):1]]
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
-- 4):1 ends here

-- [[file:chapter-exercises.org::*5)][5):1]]
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> (f x):y) []
-- 5):1 ends here

-- [[file:chapter-exercises.org::*6)][6):1]]
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x:y else y) []
-- 6):1 ends here

-- [[file:chapter-exercises.org::*7)][7):1]]
squish :: [[a]] -> [a]
squish = foldr (++) []
-- 7):1 ends here

-- [[file:chapter-exercises.org::*8)][8):1]]
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> (f x) ++ y) []
-- 8):1 ends here

-- [[file:chapter-exercises.org::*9)][9):1]]
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- 9):1 ends here

-- [[file:chapter-exercises.org::*10)][10):1]]
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\x y -> if f x y == GT then x else y) x xs
-- 10):1 ends here

-- [[file:chapter-exercises.org::*11)][11):1]]
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\x y -> if f x y == LT then x else y) x xs
-- 11):1 ends here
