module Lib where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) xs Nil = xs
  (<>) Nil xs = xs
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) as = fmap f as <> (fs <*> as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    n <- chooseInt (1, 10)
    go n (pure Nil)
      where
        go :: Arbitrary a => Int -> Gen (List a) -> Gen (List a)
        go 0 l = l
        go n l = go (n - 1) (Cons <$> arbitrary <*> l)

instance Eq a => EqProp (List a) where
  (=-=) = eq
