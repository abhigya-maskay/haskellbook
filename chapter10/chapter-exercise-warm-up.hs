-- [[file:chapter-exercises.org::*1)][1):1]]
stops = "pbtdkg"
vowels = "aeiou"
-- 1):1 ends here

-- [[file:chapter-exercises.org::*a)][a):1]]
letterCombinations :: String -> String -> [String]
letterCombinations stops vowels = [ x:y:[z] | x <- stops, y <- vowels, z <- stops]
-- a):1 ends here

-- [[file:chapter-exercises.org::*b)][b):1]]
pLetterCombinations :: String -> String -> [String]
pLetterCombinations stops vowels = [ x:y:[z] | x <- stops, y <- vowels, z <- stops, x == 'p']
-- b):1 ends here

-- [[file:chapter-exercises.org::*c)][c):1]]
nouns = ["Ocean", "Forest", "Key", "Table", "Phone", "Dream", "Whisper", "Adventure", "Door", "Song"]
verbs = ["Jump", "Consider", "Design", "Hear", "Watch", "Read", "Talk", "Construct", "Discover", "Visualize"]

wordCombinations :: [String] -> [String] -> [(String, String, String)]
wordCombinations nouns verbs = [ (x,y,z) | x <- nouns, y <- verbs, z <- nouns]
-- c):1 ends here

-- [[file:chapter-exercises.org::*2)][2):1]]
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
-- 2):1 ends here

-- [[file:chapter-exercises.org::*3)][3):1]]
seekritFunc' x = totalChars / numWords
  where
    totalChars = fromIntegral . sum . map length . words $ x
    numWords = fromIntegral . length . words $ x
-- 3):1 ends here
