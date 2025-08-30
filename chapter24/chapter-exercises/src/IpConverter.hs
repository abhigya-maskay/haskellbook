module IpConverter where
import Ipv4
import Ipv6
import Data.Word

toIpv6 :: IPAddress -> IpAddress6
toIpv6 (IPAddress ip4) = IpAddress6 0 (fromIntegral ip4)

maxIpv4 :: Word32
maxIpv4 = maxBound

toIpv4 :: IpAddress6 -> IPAddress
toIpv4 (IpAddress6 0 ip) = if fromIntegral ip > maxIpv4
  then error "Out of range"
  else IPAddress (fromIntegral ip)
toIpv4 (IpAddress6 _ _) = error "Out of range"
