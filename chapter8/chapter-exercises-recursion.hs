-- [[file:chapter-exercises.org::RTwo][RTwo]]
recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 1 = 1
recursiveSum n = n + (recursiveSum (n - 1))
-- RTwo ends here

-- [[file:chapter-exercises.org::RThree][RThree]]
recursiveMult :: (Eq a, Num a) => a -> a -> a
recursiveMult x 0 = 0
recursiveMult x y = x + (recursiveMult x (y - 1))
-- RThree ends here
