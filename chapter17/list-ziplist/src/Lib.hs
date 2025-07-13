module Lib where

import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Semigroup (ZipList' a) where
  (<>) (ZipList' xs) (ZipList' ys) = ZipList' (xs <> ys)


instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance (Eq a) => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take 3000 l
      ys' =
        let (ZipList' l) = ys
         in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat x
  (<*>) _ (ZipList' []) = ZipList' []
  (<*>) (ZipList' []) _ = ZipList' []
  (<*>) (ZipList' (f:fs)) (ZipList' (x:xs)) = ZipList' [f x] <> (ZipList' fs <*> ZipList' xs)
