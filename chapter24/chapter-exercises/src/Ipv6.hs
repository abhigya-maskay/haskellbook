{-# LANGUAGE OverloadedStrings #-}
module Ipv6 where

import Data.Attoparsec.Text
import Data.Word
import Util (leftPad, readx, showx, chunk)
import Control.Applicative
import Numeric (readHex, showHex)
import Data.List (intercalate, group)

data IpAddress6 = IpAddress6 Word64 Word64
data IpBlock = Block String | ShortenedBlock deriving (Show, Eq)

shortenGroupedBlocks :: [[String]] -> [String]
shortenGroupedBlocks = fst . foldr f ([], False) where
  f :: [String] -> ([String], Bool) -> ([String], Bool)
  f [] acc = acc
  f currBlock@(x:_) (acc, replaced) = if x == "0000" && length currBlock > 1 && not replaced then
    ("":acc, True) else
    (currBlock ++ acc, replaced)

instance Show IpAddress6 where
  show (IpAddress6 word1 word2) = str where
    hexStr1 :: String
    hexStr1 = leftPad 16 . showx showHex $ word1
    hexStr2 :: String
    hexStr2 = leftPad 16 . showx showHex $ word2
    chunked :: [String]
    chunked = chunk 4 (hexStr1 ++ hexStr2)
    str :: String
    str = intercalate ":" . shortenGroupedBlocks . group $ chunked

isBlock :: IpBlock -> Bool
isBlock (Block _) = True
isBlock _ = False

getBlock :: IpBlock -> String
getBlock (Block s) = s
getBlock _ = error "Invalid type"

parseHexChar :: Parser Char
parseHexChar = digit <|> isHexLetter where
  hexLetters :: String
  hexLetters = ['a'..'f'] ++ ['A'..'F']
  isHexLetter :: Parser Char
  isHexLetter = satisfy (inClass hexLetters)


parseBlock :: Parser IpBlock
parseBlock = Block <$> many1 parseHexChar <* optional (char ':')

parseShortenedBlock :: Parser IpBlock
parseShortenedBlock = ShortenedBlock <$ char ':'

parseSection :: Parser IpBlock
parseSection = parseBlock <|> parseShortenedBlock

expandBlocks :: [IpBlock] -> [IpBlock]
expandBlocks blocks = foldr f [] blocks where
  f :: IpBlock -> [IpBlock] -> [IpBlock]
  f (Block x) acc = let padded = leftPad 4 x in (Block padded:acc)
  f ShortenedBlock acc = let neededLength = 8 - length blocks + 1
                             newBlocks = replicate neededLength (Block "0000")
                             in newBlocks ++ acc

createIpWord :: [IpBlock] -> Word64
createIpWord = fromIntegral . readx readHex . concatMap f . filter isBlock where
  f :: IpBlock -> String
  f = getBlock

parseIpv6 :: Parser IpAddress6
parseIpv6 = fmap (makeIp . expandBlocks) . many $ parseSection where
  makeIp :: [IpBlock] -> IpAddress6
  makeIp blocks = let (first,second) = splitAt 4 blocks
    in IpAddress6 (createIpWord first) (createIpWord second)
