import Lib
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

ap :: Validation String (Int, Int, Int)
ap = undefined

main :: IO ()
main = quickBatch $ applicative ap
