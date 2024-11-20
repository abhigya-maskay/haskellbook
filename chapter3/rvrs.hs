module Reverse where

rvrs :: String -> String
rvrs x =
  concat
    [ drop 9 x,
      take 4 $ drop 5 x,
      take 5 x
    ]

rvrsResult = rvrs "Curry is awesome"

main :: IO ()
main = print rvrsResult
