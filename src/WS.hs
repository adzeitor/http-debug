{-# LANGUAGE OverloadedStrings #-}
module WS where


import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           Data.Aeson
import           Data.IORef
import qualified Data.Vector            as V
import qualified Network.WebSockets     as WS

import           Types

sendPrevious :: WS.Connection -> App -> IO ()
sendPrevious conn app = do
  logs' <- readIORef (logs app)
  V.forM_ (V.reverse logs') $ \r -> do
    WS.sendTextData conn (encode r)

application :: App -> WS.PendingConnection -> IO ()
application app pending = do
  conn <- WS.acceptRequest pending
  chan <- atomically $ dupTChan (logChan app)
  sendPrevious conn app
  forever $ do
    r <- atomically $ readTChan chan
    WS.sendTextData conn (encode r)

init :: App -> IO ()
init app = do
  let port = 9160
  putStrLn $ "ws://localhost:" ++ show port
  WS.runServer "0.0.0.0" port $ application app
