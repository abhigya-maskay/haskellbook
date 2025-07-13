import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Lib

ls :: List (String, String, String)
ls = Cons ("a", "b", "c") (Cons ("d", "e", "f") (Cons ("g", "h", "i") Nil))

main :: IO ()
main = quickBatch $ applicative ls
