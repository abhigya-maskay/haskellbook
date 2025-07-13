module InstancesSpec where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Instances

pairAp :: Pair (Int, Int, Int)
pairAp = undefined

twoAp :: Two String (Int, Int, Int)
twoAp = undefined

threeAp :: Three String String (Int, Int, Int)
threeAp = undefined

three'Ap :: Three' String (Int, Int, Int)
three'Ap = undefined

fourAp :: Four String String String (Int, Int, Int)
fourAp = undefined

four'Ap :: Four' String (Int, Int, Int)
four'Ap = undefined

testInstances :: IO ()
testInstances = do
  quickBatch $ applicative pairAp
  quickBatch $ applicative twoAp
  quickBatch $ applicative threeAp
  quickBatch $ applicative three'Ap
  quickBatch $ applicative fourAp
  quickBatch $ applicative four'Ap
