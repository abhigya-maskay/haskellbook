-- [[file:logic-goats.org::*Logic Goats][Logic Goats:1]]
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42
-- Logic Goats:1 ends here

-- [[file:logic-goats.org::*1)][1):1]]
newtype Tup = Tup (Int,String) deriving (Eq, Show)

instance TooMany Tup where
  tooMany (Tup (a,b)) = a > 42
-- 1):1 ends here

-- [[file:logic-goats.org::*2)][2):1]]
newtype TupInt = TupInt (Int,Int) deriving (Eq, Show)

instance TooMany TupInt where
  tooMany (TupInt (a,b)) = (a + b) > 42
-- 2):1 ends here

-- [[file:logic-goats.org::*3)][3):1]]
newtype ThirdTup a = ThirdTup (a,a) deriving (Eq, Show)

instance (Num a, TooMany a) => TooMany (ThirdTup a) where
  tooMany (ThirdTup (a, b)) = tooMany (a + b)
-- 3):1 ends here
