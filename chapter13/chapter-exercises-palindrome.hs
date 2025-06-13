import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlpha)

palindrome :: IO()
palindrome = forever $ do
  line1 <- getLine
  let
    processedWord = map toLower . filter isAlpha $ line1
    in
    case processedWord == reverse processedWord of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
