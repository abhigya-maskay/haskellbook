-- Maybe 1

-- [[file:chapter-exercises.org::*Maybe 1][Maybe 1:1]]
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False
-- Maybe 1:1 ends here

-- Maybe 2

-- [[file:chapter-exercises.org::*Maybe 2][Maybe 2:1]]
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def f Nothing = def
mayybee _ f (Just x) = f x
-- Maybe 2:1 ends here

-- Maybe 3

-- [[file:chapter-exercises.org::*Maybe 3][Maybe 3:1]]
fromMaybe :: a -> Maybe a -> a
fromMaybe def = mayybee def id
-- Maybe 3:1 ends here

-- Maybe 4

-- [[file:chapter-exercises.org::*Maybe 4][Maybe 4:1]]
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
-- Maybe 4:1 ends here

-- Maybe 5

-- [[file:chapter-exercises.org::*Maybe 5][Maybe 5:1]]
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList
-- Maybe 5:1 ends here

-- Maybe 6

-- [[file:chapter-exercises.org::*Maybe 6][Maybe 6:1]]
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
  | any isNothing $ xs = Nothing
  | otherwise = Just . catMaybes $ xs
-- Maybe 6:1 ends here
