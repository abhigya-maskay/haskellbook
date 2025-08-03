{-# LANGUAGE InstanceSigs #-}

module Moi where

import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype Moi s a
  = Moi {runMoi :: s -> (a, s)}

instance Show (Moi s a) where
  show _ = "func 1"

instance (Show s, Arbitrary s, EqProp a, EqProp s) => EqProp (Moi s a) where
  (Moi f) =-= (Moi f') = property $ \inputs ->
    conjoin [f x =-= f' x | x <- take 100 inputs]

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Moi s a) where
  arbitrary = Moi <$> arbitrary

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a', s') = g s in (f a', s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi {runMoi = \s -> (a, s)}

  (<*>) :: (Moi s (a -> b)) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi
      { runMoi = \s ->
          let (f', s') = f s
              (g', s'') = g s'
           in (f' g', s'')
      }

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \ s ->
    let (a',s') = f s
        moib = g a'
    in runMoi moib s'
