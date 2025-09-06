class Bifunctor p where

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f  = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id


data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux a c) = Deux (f a) (g c)

data Const a b = Const a

instance Bifunctor Const where
  bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
  bimap f g (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap :: (b -> x) -> (c -> y) -> Drei a b c -> Drei a x y
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap :: (b -> x) -> (c -> y) -> SuperDrei a b c -> SuperDrei a x y
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap :: (b -> x) -> (c -> y) -> SemiDrei a b c -> SemiDrei a x y
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap :: (c -> x) -> (d -> y) -> Quadriceps a b c d -> Quadriceps a b x y
  bimap f g (Quadzzz a b c d)  = Quadzzz a b (f c) (g d)

data Either' a b =
  Left' a | Right' b

instance Bifunctor Either' where
  bimap :: (a -> x) -> (b -> y) -> Either' a b -> Either' x y
  bimap f g (Left' a) = Left' (f a)
  bimap f g (Right' b) = Right' (g b)
