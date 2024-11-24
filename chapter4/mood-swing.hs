data Mood = Blah | Woot deriving (Show)

-- 1.The type constructor here is Mood
-- 2.The Mood values that you could possibly use are Blah and Woot
-- 3. changeMood should be a function from Mood -> Mood

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
