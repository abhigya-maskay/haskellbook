module RollYourOwn where

import RandomExample
import Control.Monad.Trans.State
import System.Random

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g where
  go sum count gen
    | sum > n = count
    | otherwise =
      let (die, nextGen) = randomR (1,6) gen
      in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 [] where
  go sum rolls gen
    | sum > n = (length rolls, reverse rolls)
    | otherwise =
      let (die, nextGen) = randomR (1,6) gen
      in go (sum + die) (intToDie die:rolls) nextGen
