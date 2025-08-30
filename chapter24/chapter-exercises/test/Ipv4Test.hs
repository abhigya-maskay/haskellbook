{-# LANGUAGE OverloadedStrings #-}

module Ipv4Test where

import qualified Data.Text as T
import Ipv4
import Test.Hspec
import TestUtil (parseResultShouldBe)

ip1 :: T.Text
ip1 = "172.16.254.1"

ip1Value :: IPAddress
ip1Value = IPAddress 2886794753

ip2 :: T.Text
ip2 = "204.120.0.15"

ip2Value :: IPAddress
ip2Value = IPAddress 3430416399

testIp4 :: IO ()
testIp4 = hspec $ do
  describe "parse Ipv4" $ do
    it "should correctly parse ip4 addresses" $ do
      parseResultShouldBe parseIpv4 ip1 ip1Value
      parseResultShouldBe parseIpv4 ip2 ip2Value
