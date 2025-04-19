-- 1) does not typecheck because the type Person does not have a defined instance of the Show typeclass
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2) does not typecheck because the datatype Mood does not have an instance of the type Eq
data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = if x == Woot then Blah else x

-- 3) a) Only values of type Mood
--    b) It wont typecheck because the function cannot take a number type
--    c) It wont typecheck because the datatype does not have an instance of the typeclass Ord

-- 4)
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
