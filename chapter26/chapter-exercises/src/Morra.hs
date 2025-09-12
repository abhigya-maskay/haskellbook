{-# LANGUAGE NamedFieldPuns #-}
module Morra where

import Data.Char
import System.Random.Stateful
import System.Exit (exitSuccess)

data Totals = Odd | Even deriving (Show, Eq)

data Choice = One | Two deriving (Show, Enum, Bounded)

data Result = PlayerWin | ComputerWin

data Choices = Choices
  { playerChoice :: !Choice,
    computerChoice :: !Choice
  }

data Score = Score
  { playerScore :: !Int,
    computerScore :: !Int
  }

totalsFromString :: String -> Maybe Totals
totalsFromString s = case s of
  "odd" -> Just Odd
  "even" -> Just Even
  _ -> Nothing

choiceFromString :: String -> Maybe Choice
choiceFromString s = case s of
  "1" -> Just One
  "2" -> Just Two
  _ -> Nothing

choiceToInt :: Choice -> Int
choiceToInt One = 1
choiceToInt Two = 2

getTotal :: Int -> Totals
getTotal n = if even n then Even else Odd

sumChoice :: Choice -> Choice -> Int
sumChoice x y = (choiceToInt x) + (choiceToInt y)

promptForPlayerTotals :: IO Totals
promptForPlayerTotals = do
  _ <- putStrLn "Pick between odds and evens. Type odd to pick odds, even to pick evens"
  choice <- fmap (totalsFromString . fmap toLower) getLine
  maybe promptForPlayerTotals return choice

promptForPlayerTurn :: IO Choice
promptForPlayerTurn = do
  _ <- putStrLn "Pick your number: (1 or 2)"
  choice <- fmap choiceFromString getLine
  maybe promptForPlayerTurn return choice

runRound :: IO Choices
runRound = do
  cChoiceInt <- (uniformRM (0,1) globalStdGen :: IO Int)
  let cChoice = toEnum cChoiceInt
  pChoice <- promptForPlayerTurn
  putStrLn $ "Computer Played: \n" ++ (show . choiceToInt $ cChoice)
  return $ Choices pChoice cChoice

decideResult :: Totals -> Choices -> Result
decideResult totals Choices {playerChoice, computerChoice} = winner where
  currentTurnSum = sumChoice playerChoice computerChoice
  currentTotal = getTotal currentTurnSum
  winner =
    if currentTotal == totals
    then PlayerWin
    else ComputerWin

processResult :: Result -> IO ()
processResult result = case result of
  PlayerWin -> putStrLn "You won!!"
  ComputerWin -> putStrLn "You lost!!"

promptForNextGame :: Totals -> IO ()
promptForNextGame totals = do
  putStrLn "Play another game (Y/N)"
  answer <- fmap (map toUpper) getLine
  case answer of
    "Y" -> runGame totals
    _ -> exitSuccess

runGame :: Totals -> IO ()
runGame totals = do
  choices <- runRound
  let result = decideResult totals choices
  processResult result
  promptForNextGame totals


main :: IO ()
main = do
  totals <- promptForPlayerTotals
  runGame totals
