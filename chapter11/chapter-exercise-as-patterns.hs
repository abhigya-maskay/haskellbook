-- [[file:chapter-exercises.org::*As Patterns][As Patterns:1]]
import Data.Char (toUpper)
import Data.List (elemIndex)

isSubSeqOf :: (Eq a) => [a] -> [a] -> Bool
isSubSeqOf subxs xs = go subxs xs (Just (-1))
  where
    go [] xs acc = True
    go (subx : subxs) xs acc =
      if currentIndex > acc
        then go subxs xs currentIndex
        else False
      where
        currentIndex = subx `elemIndex` xs
-- As Patterns:1 ends here

-- [[file:chapter-exercises.org::*As Patterns][As Patterns:2]]
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capAndPair . words
  where
    capAndPair word@(x : xs) = (word, (: xs) . toUpper $ x)
-- As Patterns:2 ends here
