-- Unfold 1

-- [[file:chapter-exercises.org::*Unfold 1][Unfold 1:1]]
import Data.Maybe (isNothing, fromJust)

myIterate :: (a -> a) -> a -> [a]
myIterate f start = start : myIterate f (f start)
-- Unfold 1:1 ends here

-- Unfold 2

-- [[file:chapter-exercises.org::*Unfold 2][Unfold 2:1]]
myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f start = if isNothing result
  then []
  else currentVal : myUnfoldr f nextVal
  where
    result = f start
    currentVal = fst . fromJust $ result
    nextVal = snd . fromJust $ result
-- Unfold 2:1 ends here

-- Unfold 3

-- [[file:chapter-exercises.org::*Unfold 3][Unfold 3:1]]
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr g
  where
    g x = Just (x, f x)
-- Unfold 3:1 ends here
