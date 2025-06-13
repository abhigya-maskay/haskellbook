type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO()
gimmePerson = do
  putStrLn "Please enter a name:"
  name <- getLine
  putStrLn "Please enter an age:"
  age <- fmap read getLine
  let person = mkPerson name age
  printMessage person
  where
    printMessage (Right p) = do
      putStrLn "Yay successfully got a person: "
      print p
    printMessage (Left e) = do
      putStrLn "Got an error making a person: "
      print e
