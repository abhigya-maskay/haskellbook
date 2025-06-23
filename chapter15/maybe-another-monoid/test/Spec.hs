import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

newtype First' a
  = First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) f@(First' (Only _)) _ = f
  (<>) _ f@(First' (Only _)) = f
  (<>) (First' Nada) (First' Nada) = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = frequency [(1, return $ First' Nada), (1, First' . Only <$> arbitrary)]

type FirstMappend' = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend')
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
