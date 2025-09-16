module WriteTheCode where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . subtract 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \ n -> do
  print $ "Hi:" ++ show n
  return $ n + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \ s -> do
  let stringS = show s
  print $ "Hi: " ++ stringS
  return (stringS, s + 1)
