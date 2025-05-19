-- [[file:chapter-exercises.org::or][or]]
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || (myOr xs)
-- or ends here

-- [[file:chapter-exercises.org::any][any]]
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = (f x) || (myAny f xs)
-- any ends here

-- [[file:chapter-exercises.org::myElem][myElem]]
myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs) = if e == x then True else myElem e xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny e xs = myAny (== e) xs
-- myElem ends here

-- [[file:chapter-exercises.org::reverse][reverse]]
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
-- reverse ends here

-- [[file:chapter-exercises.org::squish][squish]]
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)
-- squish ends here

-- [[file:chapter-exercises.org::squishMap][squishMap]]
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)
-- squishMap ends here

-- [[file:chapter-exercises.org::squishAgain][squishAgain]]
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- squishAgain ends here

-- [[file:chapter-exercises.org::max][max]]
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comparator (x:xs) = go comparator xs x
  where
    go _ [] acc = acc
    go comparator (x:xs) acc = if comparator x acc == GT
      then go comparator xs x
      else go comparator xs acc
-- max ends here

-- [[file:chapter-exercises.org::min][min]]
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comparator (x:xs) = go comparator xs x
  where
    go _ [] acc = acc
    go comparator (x:xs) acc = if comparator x acc == LT
      then go comparator xs x
      else go comparator xs acc
-- min ends here

-- [[file:chapter-exercises.org::max and min][max and min]]
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
-- max and min ends here
