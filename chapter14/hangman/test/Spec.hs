import Lib (fillInCharacter, handleGuess, Puzzle(..))
import Test.Hspec
import System.IO.Silently (silence)

testString :: String
testString = "test"

testPuzzle :: Puzzle
testPuzzle = Puzzle testString emptyFills []
  where
    emptyFills :: [Maybe Char]
    emptyFills = map (const Nothing) testString

main :: IO()
main = hspec $ do
  describe "Fill In Character" $ do
    it "Fills in a correct character" $ do
      let
        filledIn = [Just 't', Nothing, Nothing, Just 't']
        guesses = ['t']
        correctGuess = Puzzle testString filledIn guesses
        in
        fillInCharacter testPuzzle 't' `shouldBe` correctGuess
    it "Does not fill in an incorrect guess" $ do
      let
        filledIn = map (const Nothing) testString
        guesses = ['y']
        incorrectGuess = Puzzle testString filledIn guesses
        in
        fillInCharacter testPuzzle 'y' `shouldBe` incorrectGuess
  describe "Handle Guess" $ do
    it "handles repeated guess" $ do
      let
        puzzleWithGuess = fillInCharacter testPuzzle 'y'
        in
        do
          result <- silence $ handleGuess puzzleWithGuess 'y'
          result `shouldBe` puzzleWithGuess
    it "handles correct guess" $ do
      let
        filledIn = [Just 't', Nothing, Nothing, Just 't']
        guesses = ['t']
        puzzleWithGuess = Puzzle testString filledIn guesses
        in
        do
          result <- silence $ handleGuess testPuzzle 't'
          result `shouldBe` puzzleWithGuess
    it "handles incorrect guess" $ do
      let
        filledIn = map (const Nothing) testString
        guesses = ['y']
        puzzleWithGuess = Puzzle testString filledIn guesses
        in
        do
          result <- silence $ handleGuess testPuzzle 'y'
          result `shouldBe` puzzleWithGuess
