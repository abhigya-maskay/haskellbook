module IpConverterTest where

import Ipv4
import Ipv6
import Ipv4Test (ip1Value)
import Ipv6Test (getDecimalValue)
import Test.Hspec
import IpConverter

compareIpDecimals :: IPAddress -> IpAddress6 -> Bool
compareIpDecimals (IPAddress dec) ip6 =
  dec == (fromIntegral . getDecimalValue $ ip6)

ip1ValueIp6 :: IpAddress6
ip1ValueIp6 = IpAddress6 0 2886794753

testIpConversion :: IO ()
testIpConversion = hspec $ do
  it "should convert an ipv4 to ipv6" $ do
    compareIpDecimals ip1Value (toIpv6 ip1Value) `shouldBe` True
  it "should convert an ipv6 to ipv4" $ do
    compareIpDecimals ip1Value ip1ValueIp6 `shouldBe` True
  it "should roundTrip an ip" $ do
    (toIpv4 . toIpv6 $ ip1Value) `shouldBe` ip1Value
