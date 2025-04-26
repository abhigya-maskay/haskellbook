data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

phew = Papu "chases" True
-- The above will not type check because Papu needs type Rocks instead of just a String and Yeah instead of Bool

truth = Papu (Rocks "chomskydoz") (Yeah True)
-- The above will typecheck

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
-- The above will typecheck because Papu has an instance of Eq and so does Rocks and Yeah

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
-- The above will fail to typecheck because papu does not have an instance of Ord
