module Lib where

import Data.Char (toUpper)
import Test.QuickCheck

half x = x / 2

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x : xs) = (: xs) . toUpper $ x

data Fool = Fulse | Frue deriving (Eq, Show)

foolEqual :: Gen Fool
foolEqual = elements [Fulse, Frue]

foolUnequal :: Gen Fool
foolUnequal = frequency [(2, return Fulse), (1, return Frue)]
