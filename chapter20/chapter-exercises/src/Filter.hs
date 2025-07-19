module Filter where

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap f' where
  f' x = if f x then
    pure x
    else mempty
