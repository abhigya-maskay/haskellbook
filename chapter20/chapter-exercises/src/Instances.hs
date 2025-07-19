module Instances where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Constant a b = Constant b deriving (Eq, Show)

instance Eq b => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary b => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Foldable (Constant a) where
  foldMap f (Constant a) = f a


data Two a b = Two a b deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b


data Three a b c = Three a b c deriving (Eq, Show)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c


data Three' a b = Three' a b b deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'


data Four' a b = Four' a b b b deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''
