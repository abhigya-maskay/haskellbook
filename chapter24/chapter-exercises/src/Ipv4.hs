module Ipv4 where

import Data.Attoparsec.Text
import Data.Word
import Numeric (readBin, showBin)
import Util (leftPad, readx, showx, chunk)
import Data.List (intercalate)

data IPAddress = IPAddress Word32 deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress word) = str where
    binStr = leftPad 32 . showx showBin $ word
    chunked = chunk 8 binStr
    str = intercalate "." . fmap (show . readx readBin) $ chunked

createIpWord :: Integer -> Integer -> Integer -> Integer -> Word32
createIpWord = getWord
  where
    getWord :: Integer -> Integer -> Integer -> Integer -> Word32
    getWord x x' x'' x''' = getWordValue . foldMap convertToBinaryPaddedString $ [x, x', x'', x''']
    convertToBinaryPaddedString :: Integer -> String
    convertToBinaryPaddedString = leftPad 8 . showx showBin
    getWordValue :: String -> Word32
    getWordValue = fromIntegral . readx readBin

parseIpv4 :: Parser IPAddress
parseIpv4 = do
  first <- decimal
  _ <- char '.'
  second <- decimal
  _ <- char '.'
  third <- decimal
  _ <- char '.'
  fourth <- decimal
  return $ IPAddress (createIpWord first second third fourth)
