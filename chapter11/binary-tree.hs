-- [[file:binary-tree.org::*Setup][Setup:1]]
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
-- Setup:1 ends here

-- [[file:binary-tree.org::*Map][Map:1]]
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node
    (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected =
  Node
    (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup OK!"
    else error "test failed!"
-- Map:1 ends here

-- [[file:binary-tree.org::*Convert to Lists][Convert to Lists:1]]
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node a b c) = [b] ++ (preorder a) ++ (preorder c)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node a b c) = (inorder a) ++ [b] ++ (inorder c)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node a b c) = (postorder a) ++ (postorder c) ++ [b]

testTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorde fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"

main :: IO ()
main = do
  mapOkay
  testPreorder
  testInorder
  testPostorder
-- Convert to Lists:1 ends here

-- [[file:binary-tree.org::*Fold][Fold:1]]
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z t = go f z t
  where
    go f acc Leaf = acc
    go f acc (Node a b c) = f b base'
      where
        base' = foldTree f base c
        base = foldTree f acc a
-- Fold:1 ends here
