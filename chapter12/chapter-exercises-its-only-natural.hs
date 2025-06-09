-- It's Only Natural

-- #+NAME natural

-- [[file:chapter-exercises.org::*It's Only Natural][It's Only Natural:1]]
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat n
  | n < 0 = Nothing
  | n > 0 = fmap Succ . integerToNat $ (n - 1)
-- It's Only Natural:1 ends here
