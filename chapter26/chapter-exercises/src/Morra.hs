{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Morra (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify')
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (lift)
import Data.Char (toLower, isSpace)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)
import Numeric.Natural (Natural)
import qualified Data.Map.Strict as M

data Parity = Odd | Even deriving (Show, Eq)

newtype PlayerScore = PlayerScore Natural
  deriving (Eq, Show, Num)

data Choice = One | Two deriving (Enum, Bounded, Eq, Ord)

data GameMode
  = PvP
  | PvE !AIMemory !MoveHistory
  deriving (Eq)

data GameAction = Continue | Quit deriving (Eq)

data PlayerTag = Player1 | Player2

data MoveHistory
  = NoMoves
  | OneMove !Choice
  | TwoMoves !Choice !Choice
  deriving (Eq, Ord)

type AIMemory = M.Map MoveHistory Choice

data RoundChoices = RoundChoices
  { player1Choice :: !Choice,
    player2Choice :: !Choice
  }

data Score = Score
  { player1Score :: !PlayerScore,
    player2Score :: !PlayerScore
  }

data GameConfig = GameConfig
  { player1Parity :: !Parity
  }

data GameState = GameState
  { score :: !Score,
    gameMode :: !GameMode
  }

type GameM = StateT GameState (ReaderT GameConfig IO)

-- | Read the configured parity choice for player one.
askParity :: GameM Parity
askParity = lift $ asks player1Parity

-- | Build the initial application state for a given starting mode.
mkInitialState :: GameMode -> GameState
mkInitialState mode = GameState
  { score = Score 0 0
  , gameMode = mode
  }

-- | Construct the read-only game configuration.
mkGameConfig :: Parity -> GameConfig
mkGameConfig parity = GameConfig
  { player1Parity = parity
  }

-- | Prompt shown when selecting how to play.
msgChooseGameMode :: String
msgChooseGameMode = unlines
  [ "Choose game mode:",
    "Press 1 to play against a computer",
    "Press 2 to play against another player"
  ]

-- | Message asking a player to choose odds or evens.
msgPickOddsEvens :: GameMode -> String
msgPickOddsEvens mode =
  getPlayerName mode Player1 ++ " Pick between odds and evens. Type odd to pick odds, even to pick evens"

-- | Message prompting a specific player to pick a number.
msgPickNumber :: String -> String
msgPickNumber playerName = "Pick your number " ++ playerName ++ ": (1 or 2)"

-- | Message inviting the players to continue the session.
msgPlayAgain :: String
msgPlayAgain = "Play another round (y/n)"


-- | Message reporting that a particular player has won.
msgPlayerWon :: GameMode -> PlayerTag -> String
msgPlayerWon mode player = getPlayerName mode player ++ " Won!!"

-- | Fixed prefix for score readouts.
msgCurrentScore :: String
msgCurrentScore = "Current Score is:"

-- | Resolve a user-facing name for the supplied player tag.
getPlayerName :: GameMode -> PlayerTag -> String
getPlayerName PvP Player1 = "Player1"
getPlayerName PvP Player2 = "Player2"
getPlayerName PvE {} Player1 = "Player"
getPlayerName PvE {} Player2 = "Computer"

-- | Helpful tuple of player labels for the current mode.
playerLabels :: GameMode -> (String, String)
playerLabels mode = (getPlayerName mode Player1, getPlayerName mode Player2)

-- | Determine whether an integer is odd or even.
parityOf :: Int -> Parity
parityOf n = if even n then Even else Odd

-- | Remove leading and trailing whitespace from user input.
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- | Case-insensitive lookup helper shared by the various parsers.
parseFromTable :: [(String, a)] -> String -> Maybe a
parseFromTable table = flip lookup table . map toLower . trim

-- | Parse the odds-or-evens selection typed by the player.
parseParity :: String -> Maybe Parity
parseParity = parseFromTable [("o", Odd), ("odd", Odd), ("e", Even), ("even", Even)]

-- | Parse a numeric turn choice.
parseChoice :: String -> Maybe Choice
parseChoice = parseFromTable [("1", One), ("2", Two)]

-- | Parse the menu selection for game mode.
parseGameMode :: String -> Maybe GameMode
parseGameMode = parseFromTable
  [ ("1", PvE M.empty NoMoves)
  , ("2", PvP)
  ]

-- | Parse whether the players wish to continue or stop.
parseGameAction :: String -> Maybe GameAction
parseGameAction = parseFromTable [("y", Continue), ("yes", Continue), ("n", Quit), ("no", Quit)]

-- | Convert an enumerated choice to its numeric representation.
choiceToInt :: Choice -> Int
choiceToInt = (+ 1) . fromEnum

-- | Sum the numeric values of the two moves played.
sumChoices :: Choice -> Choice -> Int
sumChoices x y = choiceToInt x + choiceToInt y

-- | Keep prompting the user until the parser succeeds.
promptUntilJust :: String -> (String -> Maybe a) -> IO a
promptUntilJust msg parse = do
  putStrLn msg
  input <- getLine
  case parse input of
    Just x -> pure x
    Nothing -> do
      putStrLn "Invalid input, try again."
      promptUntilJust msg parse

-- | Request the odds-or-evens choice for player one.
promptForParity :: GameMode -> IO Parity
promptForParity mode = promptUntilJust (msgPickOddsEvens mode) parseParity

-- | Ask a named player to enter their move.
promptForPlayerTurn :: String -> IO Choice
promptForPlayerTurn playerName = promptUntilJust (msgPickNumber playerName) parseChoice

-- | Ask whether the players want to play another round.
promptForNextGame :: IO GameAction
promptForNextGame = promptUntilJust msgPlayAgain parseGameAction

-- | Ask whether the session will be PvP or PvE.
promptForGameMode :: IO GameMode
promptForGameMode = promptUntilJust msgChooseGameMode parseGameMode

-- | Produce human-readable lines describing the two moves.
formatChoices :: GameMode -> RoundChoices -> [String]
formatChoices mode (RoundChoices p1 p2) =
  let (p1Name, p2Name) = playerLabels mode
   in [ p1Name ++ " played: " ++ show (choiceToInt p1)
      , p2Name ++ " played: " ++ show (choiceToInt p2)
      ]

-- | Print the latest pair of moves to the terminal.
displayChoices :: GameMode -> RoundChoices -> IO ()
displayChoices mode choices = mapM_ putStrLn (formatChoices mode choices)

-- | Sample a random move for the computer opponent.
randomMove :: IO Choice
randomMove = toEnum <$> randomRIO (fromEnum (minBound :: Choice), fromEnum (maxBound :: Choice))

-- | Select the AI move, preferring a learned counter over randomness.
nextMove :: AIMemory -> MoveHistory -> IO Choice
nextMove aiMemory moveHistory =
  maybe randomMove pure (M.lookup moveHistory aiMemory)

-- | Retrieve the computer's move using the active mode and memory.
getComputerMove :: GameM Choice
getComputerMove = do
  mode <- gets gameMode
  case mode of
    PvP -> liftIO randomMove
    PvE memory moves -> liftIO $ nextMove memory moves

-- | Extend the bounded history with the most recent human move.
addMove :: Choice -> MoveHistory -> MoveHistory
addMove choice NoMoves = OneMove choice
addMove choice (OneMove prior) = TwoMoves choice prior
addMove choice (TwoMoves recent _older) = TwoMoves choice recent

-- | Swap a move from one to two or vice versa.
flipChoice :: Choice -> Choice
flipChoice One = Two
flipChoice Two = One

-- | Decide which move the AI should play to win given a parity target.
computeOptimalCounter :: Parity -> Choice -> Choice
computeOptimalCounter Even = flipChoice
computeOptimalCounter Odd = id

-- | Record the human move and winning reply for future rounds.
processPvEChoice :: Parity -> Choice -> AIMemory -> MoveHistory -> (AIMemory, MoveHistory)
processPvEChoice parity pChoice memory history =
  let newHistory = addMove pChoice history
      winningMove = computeOptimalCounter parity pChoice
      newMemory = M.insert history winningMove memory
  in (newMemory, newHistory)

-- | Update the AI memory after observing the human move.
updateAIMemoryPvE :: Choice -> GameM ()
updateAIMemoryPvE pChoice = do
  parity <- askParity
  mode <- gets gameMode
  case mode of
    PvP -> pure ()
    PvE aiMemory moveHistory -> do
      let (newMemory, newHistory) = processPvEChoice parity pChoice aiMemory moveHistory
      modify' $ \gs -> gs { gameMode = PvE newMemory newHistory }

-- | Run a round between a human and the adaptive AI.
runPvERound :: GameM RoundChoices
runPvERound = do
  mode <- gets gameMode
  let (p1Name, _) = playerLabels mode
  cChoice <- getComputerMove
  (choices, pChoice) <- liftIO $ do
    pChoice <- promptForPlayerTurn p1Name
    let choices = RoundChoices pChoice cChoice
    displayChoices mode choices
    pure (choices, pChoice)
  updateAIMemoryPvE pChoice
  pure choices

-- | Prompt for a move while hiding it from the other player.
promptSecretChoice :: String -> IO Choice
promptSecretChoice name = do
  c <- promptForPlayerTurn name
  clearScreen
  pure c

-- | Run a round between two human players.
runPvPRound :: GameM RoundChoices
runPvPRound = do
  mode <- gets gameMode
  let (p1Name, p2Name) = playerLabels mode
  liftIO $ do
    clearScreen
    p1Choice <- promptSecretChoice p1Name
    p2Choice <- promptSecretChoice p2Name
    let choices = RoundChoices p1Choice p2Choice
    displayChoices mode choices
    pure choices

-- | Dispatch to the correct gameplay loop for the current mode.
runRound :: GameM RoundChoices
runRound = do
  mode <- gets gameMode
  case mode of
    PvP -> runPvPRound
    PvE {} -> runPvERound

-- | Work out who won the round based on the parity bet.
decideWinner :: RoundChoices -> Parity -> PlayerTag
decideWinner RoundChoices {player1Choice, player2Choice} expectedParity =
  if parityOf (sumChoices player1Choice player2Choice) == expectedParity
    then Player1
    else Player2

-- | Increment the appropriate player's score after a win.
updateScore :: PlayerTag -> Score -> Score
updateScore Player1 score = score { player1Score = player1Score score + 1 }
updateScore Player2 score = score { player2Score = player2Score score + 1 }

-- | Inform the players who won the round.
printResult :: GameMode -> PlayerTag -> IO ()
printResult mode player = putStrLn (msgPlayerWon mode player)

-- | Render the current score for display.
formatScore :: GameMode -> Score -> [String]
formatScore mode Score {player1Score, player2Score} =
  let (p1Name, p2Name) = playerLabels mode
      PlayerScore p1 = player1Score
      PlayerScore p2 = player2Score
   in [ msgCurrentScore
      , p1Name ++ " : " ++ show p1
      , p2Name ++ " : " ++ show p2
      ]

-- | Print the formatted scoreboard to the terminal.
printScore :: GameMode -> Score -> IO ()
printScore mode score = mapM_ putStrLn (formatScore mode score)

-- | Compute winner, total, and parity from a completed round.
calculateRoundOutcome :: RoundChoices -> Parity -> (PlayerTag, Int, Parity)
calculateRoundOutcome choices expectedParity =
  let s = sumChoices (player1Choice choices) (player2Choice choices)
      par = parityOf s
      winner = decideWinner choices expectedParity
   in (winner, s, par)

-- | Display-friendly description of the summed turn value.
formatRoundResult :: Int -> Parity -> String
formatRoundResult total actualParity = "Sum: " ++ show total ++ " (" ++ show actualParity ++ ")"

-- | Apply round outcomes to state and surface the results.
processRound :: RoundChoices -> GameM ()
processRound choices = do
  expectedParity <- askParity
  mode <- gets gameMode
  let (winner, total, actualParity) = calculateRoundOutcome choices expectedParity
  modify' $ \gs -> gs { score = updateScore winner (score gs) }
  newScore <- gets score
  liftIO $ do
    putStrLn $ formatRoundResult total actualParity
    printResult mode winner
    printScore mode newScore

-- | Tail-recursive main loop that runs successive rounds.
gameLoop :: GameM ()
gameLoop = do
  choices <- runRound
  processRound choices
  action <- liftIO promptForNextGame
  when (action == Continue) gameLoop

-- | Program entry point wiring configuration, state, and loop.
main :: IO ()
main = do
  initialMode <- promptForGameMode
  chosenParity <- promptForParity initialMode
  let config = mkGameConfig chosenParity
      initialState = mkInitialState initialMode
  runReaderT (evalStateT gameLoop initialState) config
