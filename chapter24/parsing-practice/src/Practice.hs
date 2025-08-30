module Practice where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

one'' :: Parser Char
one'' = do
  a <- one
  eof
  return a

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser  Char
oneTwo' = oneTwo >> stop

oneTwo'' :: Parser Char
oneTwo'' = do
  a <- oneTwo
  eof
  return a

oneTwoThree :: Parser String
oneTwoThree = choice [string "123", string "12", string "1"]

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParseString :: Parser String -> String -> IO ()
testParseString p s = print $ parseString p mempty s
