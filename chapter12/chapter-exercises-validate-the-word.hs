-- Validate the word

-- #+NAME validate

-- [[file:chapter-exercises.org::*Validate the word][Validate the word:1]]
import Data.List (partition)

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = isValidWord
  where
    characters = partition (`elem` "aeiou") s
    vowels = fst characters
    consonants = snd characters
    isValidWord = if length vowels > length consonants
      then Nothing
      else Just . Word' $ s
-- Validate the word:1 ends here
