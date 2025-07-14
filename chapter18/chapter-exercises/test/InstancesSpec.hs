module InstancesSpec where
import Instances
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

nopem :: Nope (Int, Int, Int)
nopem = undefined

bahEitherm :: BahEither String (Int, Int, Int)
bahEitherm = undefined

identitym :: Identity (Int, Int, Int)
identitym = undefined

listm :: List (Int, Int, Int)
listm = undefined

testInstances :: IO ()
testInstances = do
  quickBatch $ monad nopem
  quickBatch $ monad bahEitherm
  quickBatch $ monad identitym
  quickBatch $ monad listm
