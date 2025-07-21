module Main (main) where

import Instances
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type TraverseTest = (Maybe Int, Maybe Int, Int, String)

identityTraverse :: Identity TraverseTest
identityTraverse = undefined

constantTraverse :: Constant String TraverseTest
constantTraverse = undefined

optionalTraverse :: Optional TraverseTest
optionalTraverse = undefined

listTraverse :: List TraverseTest
listTraverse = undefined

threeTraverse :: Three Int Int TraverseTest
threeTraverse = undefined

pairTraverse :: Pair Int TraverseTest
pairTraverse = undefined

bigTraverse :: Big Int TraverseTest
bigTraverse = undefined

biggerTraverse :: Bigger Int TraverseTest
biggerTraverse = undefined

snTraverse :: S [] TraverseTest
snTraverse = undefined

treeTraverse :: Tree TraverseTest
treeTraverse = undefined

main :: IO ()
main = do
  quickBatch $ traversable identityTraverse
  quickBatch $ traversable constantTraverse
  quickBatch $ traversable optionalTraverse
  quickBatch $ traversable listTraverse
  quickBatch $ traversable threeTraverse
  quickBatch $ traversable pairTraverse
  quickBatch $ traversable bigTraverse
  quickBatch $ traversable biggerTraverse
  quickBatch $ traversable snTraverse
  quickBatch $ traversable treeTraverse
