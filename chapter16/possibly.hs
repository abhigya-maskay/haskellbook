data Possibly a = LolNope | Yeppers a deriving (Show, Eq)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)
