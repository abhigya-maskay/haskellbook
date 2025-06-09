-- Tree 1

-- [[file:chapter-exercises.org::*Tree 1][Tree 1:1]]
import Data.Maybe (isNothing, fromJust)

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f start = if isNothing result
  then Leaf
  else Node leftTree currentVal rightTree
  where
    result = f start
    (leftVal,currentVal,rightVal) = fromJust result
    leftTree = unfold f leftVal
    rightTree = unfold f rightVal
-- Tree 1:1 ends here

-- Tree 2

-- [[file:chapter-exercises.org::*Tree 2][Tree 2:1]]
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0 where
  f n'
    | n' == n = Nothing
    | otherwise = Just (n' + 1, n', n'+ 1)
-- Tree 2:1 ends here
