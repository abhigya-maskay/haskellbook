import Control.Monad.Trans.Class

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

applyToFirst :: (a -> b) -> (a, s) -> (b, s)
applyToFirst f (a, s) = (f a, s)

instance (Functor m) => Functor (StateT s m) where
  fmap :: (Functor m) => (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT a) = StateT (fmap (applyToFirst f) . a)

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT f) <*> (StateT a) = StateT $ \s -> do
    (f, s') <- f s
    (a, s'') <- a s'
    return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    (runStateT . f $ a) s'

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \ s -> (, s) <$> ma
