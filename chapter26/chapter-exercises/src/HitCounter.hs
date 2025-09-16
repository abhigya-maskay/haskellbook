{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
import Control.Monad.IO.Class (MonadIO(liftIO))

data Config
  = Config
  { counts :: IORef (M.Map Text Integer),
    prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (newMap, newInt)
  where
    newMap = M.insertWith (+) k 1 m
    newInt = fromMaybe 1 . M.lookup k $ newMap

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  prefixText <- lift $ asks prefix
  countsMapRef <- lift $ asks counts
  countsMap <- liftIO (readIORef countsMapRef)
  let key' = mappend prefixText unprefixed
      (newMap, newInteger) = bumpBoomp key' countsMap
  _ <- liftIO $ writeIORef countsMapRef newMap
  html $
    mconcat
      [ "<h1>Success! Count was: ",
        TL.pack $ show newInteger,
        "</h1>"
      ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR action = runReaderT action config
  scottyT 3000 runR app
