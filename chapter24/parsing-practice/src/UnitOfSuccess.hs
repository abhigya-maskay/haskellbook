module UnitOfSuccess where

import Text.Trifecta

parse123 :: Parser Integer
parse123 = do
  int <- integer
  eof
  return int
