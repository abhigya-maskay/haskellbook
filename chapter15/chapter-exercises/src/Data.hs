module Data where

import Test.QuickCheck hiding (Failure, Success)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) (BoolConj False) _ = BoolConj False
  (<>) _ (BoolConj False) = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj True) _ = BoolDisj True
  (<>) _ (BoolDisj True) = BoolDisj True
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) x@(Snd _) _ = x
  (<>) _ x@(Snd _) = x
  (<>) _ x@(Fst _) = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine new
    where
      new n = f n <> g n

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine f where f _ = mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

newtype Comp a = Comp {unComp :: (a -> a)}

instance (Semigroup a) => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f . g)

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp f where f a = a
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  (<>) x@(Success _) _ = x
  (<>) _ x@(Success _) = x
  (<>) (Failure a) (Failure b) = Failure $ a <> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [(1, Success <$> arbitrary), (2, Failure <$> arbitrary)]

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance (Semigroup a) => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem f') = Mem final
    where
      final s = ((fst . f $ s) <> (fst . f' $ s), snd . f' . snd . f $ s)

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem empty where
    empty s = (mempty, s)
  mappend = (<>)
