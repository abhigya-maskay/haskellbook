-- String-1
-- #+NAME string-1

-- [[file:chapter-exercises.org::*String-1][String-1:1]]
import Data.Maybe (fromMaybe)

notThe :: String -> Maybe String
notThe s = if s /= "the" then Just s else Nothing

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words
-- String-1:1 ends here

-- String-2
-- #+NAME string-2

-- [[file:chapter-exercises.org::*String-2][String-2:1]]
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countVowels . getVowelThes . words
  where
    getVowelThes :: [String] -> String
    getVowelThes [] = []
    getVowelThes [x] = []
    getVowelThes ("the":(y:ys):xs) = getVowelThes ((y:ys):xs) ++ [y]
    getVowelThes (x:xs) = getVowelThes xs
-- String-2:1 ends here

-- String-3
-- #+NAME string-3

-- [[file:chapter-exercises.org::*String-3][String-3:1]]
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` "aeiou")
-- String-3:1 ends here
