-- [[file:chapter-exercises.org::*Writing instances][Writing instances:1]]
{-# LANGUAGE FlexibleInstances #-}
data Quant a b = Finance | Desk a | Bloor b deriving (Show, Eq)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)
-- Writing instances:1 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:2]]
data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a
-- Writing instances:2 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:3]]
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))
-- Writing instances:3 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:4]]
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst (f a)
-- Writing instances:4 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:5]]
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (f <$> x)
-- Writing instances:5 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:6]]
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (f <$> x) (f <$> y)
-- Writing instances:6 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:7]]
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (f <$> y)
-- Writing instances:7 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:8]]
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (f <$> z)
-- Writing instances:8 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:9]]
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
-- Writing instances:9 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:10]]
data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (f <$> x) (f <$> y)  (f <$> z)
-- Writing instances:10 ends here

-- [[file:chapter-exercises.org::*Writing instances][Writing instances:11]]
data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read g) = Read (f <$> g)
-- Writing instances:11 ends here
