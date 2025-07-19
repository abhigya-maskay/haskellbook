module InstancesSpec where

import Data.Monoid (Sum)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Instances

constantFold :: Constant (Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
constantFold = undefined

twoFold :: Two (Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
twoFold = undefined

threeFold :: Three (Sum Int) (Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
threeFold = undefined

three'Fold :: Three' (Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
three'Fold = undefined

four'Fold :: Four' (Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
four'Fold = undefined

testInstances :: IO ()
testInstances = do
  quickBatch $ foldable constantFold
  quickBatch $ foldable twoFold
  quickBatch $ foldable threeFold
  quickBatch $ foldable three'Fold
  quickBatch $ foldable four'Fold
