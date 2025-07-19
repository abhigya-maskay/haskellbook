import Data.Monoid (Any (..), Product (..), Sum (..), getAny, getProduct, getSum)

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = getAny . foldMap isElem
  where
    isElem = Any . (== e)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs
  | length xList == 0 = Nothing
  | otherwise = foldr (liftA2 min) maybeFirst maybeRest
  where
    xList = toList xs
    (xFirst : xRest) = xList
    maybeFirst = Just xFirst
    maybeRest  = Just <$> xRest

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs
  | length xList == 0 = Nothing
  | otherwise = foldr (liftA2 max) maybeFirst maybeRest
  where
    xList = toList xs
    (xFirst : xRest) = xList
    maybeFirst = Just xFirst
    maybeRest = Just <$> xRest

null :: (Foldable t) => t a -> Bool
null = (== 0) . getSum . foldMap (Sum . const 1)

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (Sum . const 1)

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr f' mempty
  where
    f' curr acc = f curr <> acc
