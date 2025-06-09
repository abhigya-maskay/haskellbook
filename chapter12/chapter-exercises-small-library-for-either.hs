-- Either 1

-- [[file:chapter-exercises.org::*Either 1][Either 1:1]]
lefts' :: [Either a b] -> [a]
lefts' = foldr addLeft []
  where
    addLeft (Left a) acc = a:acc
    addLeft (Right b) acc = acc
-- Either 1:1 ends here

-- Either 2

-- [[file:chapter-exercises.org::*Either 2][Either 2:1]]
rights' :: [Either a b] -> [b]
rights' = foldr addRight []
  where
    addRight (Left a) acc = acc
    addRight (Right b) acc = b:acc
-- Either 2:1 ends here

-- Either 3

-- [[file:chapter-exercises.org::*Either 3][Either 3:1]]
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)
-- Either 3:1 ends here

-- Either 4

-- [[file:chapter-exercises.org::*Either 4][Either 4:1]]
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)
-- Either 4:1 ends here

-- Either 5

-- [[file:chapter-exercises.org::*Either 5][Either 5:1]]
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b
-- Either 5:1 ends here

-- Either 6

-- [[file:chapter-exercises.org::*Either 6][Either 6:1]]
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
-- Either 6:1 ends here
