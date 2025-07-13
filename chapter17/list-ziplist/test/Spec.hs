import Lib
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

ls :: ZipList' (Int, Int, Int)
ls = ZipList' [(1, 2, 3)]

main :: IO ()
main = quickBatch $ applicative ls
