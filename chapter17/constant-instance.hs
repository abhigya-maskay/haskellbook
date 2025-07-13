newtype Constant a b = Constant { getConstant :: a } deriving (Show, Eq)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) _ (Constant a) = Constant a
