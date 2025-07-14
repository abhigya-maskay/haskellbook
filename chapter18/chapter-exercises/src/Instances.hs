module Instances where
import Test.QuickCheck
import Test.QuickCheck.Checkers

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = frequency [(1, PLeft <$> arbitrary), (1, PRight <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft (f a)

instance Applicative (BahEither b) where
  pure = PLeft
  (PRight a) <*> _ = PRight a
  _ <*> (PRight a) = PRight a
  (PLeft f) <*> (PLeft b) = PLeft (f b)

instance Monad (BahEither b) where
  return = pure
  (PRight a) >>= _ = PRight a
  (PLeft b) >>= f = f b

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a

data List a = Nil | Cons a (List a) deriving (Eq, Show)

toCustomList :: [a] -> List a
toCustomList [] = Nil
toCustomList (x:xs) = Cons x (toCustomList xs)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x:(toList xs)

instance Semigroup (List a) where
  Nil <> xs = xs
  xs <> Nil = xs
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
        size <- chooseInt (0,10)
        go size (return Nil)
          where
            go 0 xs = xs
            go size xs = go (size - 1) (Cons <$> arbitrary <*> xs)

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys' where
    xs' = take 3000 . toList $ xs
    ys' = take 3000 . toList $ ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  Nil >>= _ = Nil
  (Cons x xs) >>= f = f x <> (xs >>= f)
