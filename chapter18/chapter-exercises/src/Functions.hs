module Functions where

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a' b = f <$> a' <*> b

a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> first <*> rest
  where
    first = f x
    rest = meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id
