module Main (main) where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Moi

moiFunctor :: Moi Int (Int, Int, Int)
moiFunctor = undefined

moiApplicative :: Moi Int (Int, Int, Int)
moiApplicative = undefined

moiMonad :: Moi Int (Int, Int, Int)
moiMonad = undefined

main :: IO ()
main = do
  quickBatch $ functor moiFunctor
  quickBatch $ applicative moiApplicative
  quickBatch $ monad moiMonad
