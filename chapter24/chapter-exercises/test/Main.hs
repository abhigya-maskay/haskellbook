module Main (main) where

import LogFileTest
import Ipv4Test
import Ipv6Test
import IpConverterTest
import DotTest

main :: IO ()
main = do
  testLogFile
  testIp4
  testIpv6
  testIpConversion
  testDot
