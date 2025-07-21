{-# LANGUAGE FlexibleInstances #-}

module Instances where

import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype Identity a = Identity a deriving (Ord, Eq, Show)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

newtype Constant a b = Constant {getConstant :: a} deriving (Ord, Eq, Show)

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant _ <*> Constant x = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure . Constant $ x

data Optional a = Nada | Yep a deriving (Eq, Show, Ord)

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (1, Yep <$> arbitrary)]

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  _ <*> Nada = Nada
  Yep f <*> Yep x = Yep . f $ x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = (Yep <$>) . f $ x

data List a = Nil | Cons a (List a) deriving (Eq, Show, Ord)

takeList :: Int -> List a -> List a
takeList _ Nil = Nil
takeList size xs = go size xs Nil
  where
    go 0 _ acc = acc
    go _ Nil acc = acc
    go size' (Cons x' xs') acc = go (size' - 1) xs' (Cons x' acc)

instance (Eq a) => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = takeList 30 xs
      ys' = takeList 30 ys

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    int <- chooseInt (0, 10)
    go int . pure $ Nil
    where
      go 0 acc = acc
      go size acc = go (size - 1) (Cons <$> arbitrary <*> acc)

instance Semigroup (List a) where
  Nil <> xs = xs
  xs <> Nil = xs
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

data Three a b c = Three a b c deriving (Eq, Show, Ord)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three f g h) <*> (Three a b c) = Three (f <> a) (g <> b) (h c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Pair a b = Pair a b deriving (Eq, Show, Ord)

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance (Monoid a) => Applicative (Pair a) where
  pure = Pair mempty
  (Pair a f) <*> (Pair a' b) = Pair (a <> a') (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance (Monoid a) => Applicative (Big a) where
  pure x = Big mempty x x
  (Big a f f') <*> (Big a' b b') = Big (a <> a') (f b) (f' b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance (Monoid a) => Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  (Bigger a f f' f'') <*> (Bigger a' b b' b'') = Bigger (a <> a') (f b) (f' b') (f'' b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance (Traversable n) => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show, Ord)

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    size <- chooseInt (0, 10)
    go size (Leaf <$> arbitrary)
      where
        go 0 _ = return Empty
        go 1 acc = Node <$> acc <*> arbitrary <*> acc
        go size acc = go (size - 1) (Node <$> acc <*> arbitrary <*> acc)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node t x t') = Node (f <$> t) (f x) (f <$> t')

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node t x t') = foldMap f t <> f x <> foldMap f t'

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node t x t') = Node <$> traverse f t <*> f x <*> traverse f t'
