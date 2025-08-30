{-# LANGUAGE OverloadedStrings #-}

module Ipv6Test where

import qualified Data.Text as T
import Test.Hspec
import TestUtil (parseResultShouldBe)
import Ipv6
import Numeric (showHex, readHex)
import Data.Attoparsec.Text
import Util (leftPad, showx, readx)

ip1 :: T.Text
ip1 = "0:0:0:0:0:ffff:ac10:fe01"

ip1Value :: Integer
ip1Value = 281473568538113

ip2 :: T.Text
ip2 = "0:0:0:0:0:ffff:cc78:f"

ip2Value :: Integer
ip2Value = 281474112159759

ip3 :: T.Text
ip3 = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"

ip3Value :: Integer
ip3Value = 338288524927261089654163772891438416681

ip4 :: T.Text
ip4 = "2001:DB8::8:800:200C:417A"

ip4Value :: Integer
ip4Value = 42540766411282592856906245548098208122

getDecimalValue :: IpAddress6 -> Integer
getDecimalValue (IpAddress6 f s) = dec where
  fHex = leftPad 16 . showx showHex $ f
  sHex = leftPad 16 . showx showHex $ s
  bin = fHex ++ sHex
  dec = readx readHex bin

decimalParser :: Parser Integer
decimalParser = getDecimalValue <$> parseIpv6

testIpv6 :: IO ()
testIpv6 = hspec $ do
  describe "parseSection" $ do
    it "should parse a fully formed section" $ do
      parseResultShouldBe parseSection "FE80" (Block "FE80")
    it "should parse a shortened section" $ do
      parseResultShouldBe parseSection "::" (ShortenedBlock)
    it "should not change non shortened full blocks" $ do
      expandBlocks [Block "000A", Block "0012"] `shouldBe` [Block "000A", Block "0012"]
    it "should pad non padded blocks" $ do
      expandBlocks [Block "A", Block "12"] `shouldBe` [Block "000A", Block "0012"]
    it "should expand shortened blocks" $ do
      let testValue = expandBlocks [Block "A123", ShortenedBlock]
          expected = Block "A123" : replicate 7 (Block "0000")
      testValue `shouldBe` expected
    it "should create Word values from blocks" $ do
      createIpWord [Block "00AA", Block "0012"] `shouldBe` 11141138
    it "should parse an ipv6 with all sections" $ do
      parseResultShouldBe decimalParser ip1 ip1Value
      parseResultShouldBe decimalParser ip2 ip2Value
      parseResultShouldBe decimalParser ip3 ip3Value
    it "should parse an ipv6 with shortened sections" $ do
      parseResultShouldBe decimalParser ip4 ip4Value
