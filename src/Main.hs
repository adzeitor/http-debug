{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Builder    as BL
import           Data.IORef
import           Data.List                       (find)
import           Data.Maybe                      (listToMaybe)
import           Data.Monoid                     ((<>))
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Encoding         as TL
import           Data.Time                       (getCurrentTime)
import qualified Data.Vector                     as V
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp        as Warp
import qualified Network.Wai.Handler.WebSockets  as Warp
import           Network.Wai.Middleware.HttpAuth
import           Network.Wai.Parse               (FileInfo (..), lbsBackEnd,
                                                  parseRequestBody)
import qualified Network.WebSockets              as WS
import           System.Environment              (lookupEnv)
import           System.Random
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Read                       (readMaybe)

import           Types
--- TODO:...
import           View.Main
import qualified WS

router :: App -> Application
router app rq respond =
  case rawPathInfo rq of
   "/"           -> staticFileApp "www/index.html"  rq respond
   "/elm.js"     -> staticFileApp "www/elm.js" rq respond
   "/favicon.ico"     -> staticFileApp "www/favicon.ico"  rq respond
   "/tools"      -> toolsApp app rq respond
   "/bad"      -> badApp app rq respond
   "/time"     -> timeApp app rq respond
   "/upload"   -> uploadApp app rq respond
   path | "/file" `BS.isPrefixOf` path
          -> getFileApp app rq respond
   "/mimic"    -> mimicApp app rq respond
   "/auth"     -> do
     let a = basicAuth (\u p -> return $ u == "root" && p == "toor") "root:toor"  (timeApp app)
     a rq respond
   _              -> mimicApp app rq respond





staticFileApp :: String -> Application
staticFileApp file _ respond = do
  respond $ responseFile status200 [] file Nothing


toolsApp :: App -> Application
toolsApp app _ respond = do
  cnt  <- readIORef (hits app)
  uniq <- readIORef (hosts app)
  logs' <- readIORef (logs app)
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (renderHtml (mainPage (V.toList logs') cnt uniq ))


logEvent :: App -> Request -> IO Log
logEvent app rq = logEventWithFiles app rq Nothing

logEventWithFiles :: App
                  -> Request
                  -> Maybe [MyFileInfo]
                  -> IO Log
logEventWithFiles app rq files = do
  modifyIORef' (hits app) (+1)

  let remote = show (remoteHost rq)
  modifyIORef' (hosts app) (\set -> Set.insert remote set)

  body   <- requestBody rq
  now    <- getCurrentTime
  nextId <- atomicModifyIORef' (nextLogId app) (\x -> (x+1,x))
  let log' = Log { request   = rq
                 , rawBody   = body
                 , createdAt = now
                 , logId     = nextId
                 , logFiles  = files}
  modifyIORef' (logs app)  (\list -> V.force (V.take (maxLogSize app) (V.cons log' list)))
  atomically $ writeTChan (logChan app) log'
  -- FIXME: return body for queries that needs it
  return log'


uploadApp :: App -> Application
uploadApp  app rq respond = do
  a <- (Prelude.map snd . snd) `fmap` parseRequestBody lbsBackEnd rq
  l <- logEventWithFiles app rq (Just a)
  let first = Prelude.head a
      location = ("/file/" <> show (logId l) <> "/" <> BS.unpack (fileName first))

  respond $ responseLBS status301
    [ ("Content-Type", "text/plain")
    , ("Location", T.encodeUtf8(T.pack location)) ] "Redirect"

getFileApp :: App -> Application
getFileApp app rq respond = do
  logs' <- readIORef (logs app)
  let r = do
        _id <- (readMaybe . T.unpack)
               =<< listToMaybe (drop 1 (pathInfo rq))
        let _name = T.encodeUtf8 . T.concat . drop 2 $ pathInfo rq
        V.find (\x -> _id == logId x)  logs'
             >>= logFiles
             >>= find ((==_name) . fileName)

  case r of
   Just val ->
     respond $ responseLBS
        status200
        [("Content-Type", fileContentType val)]
        (fileContent val)
   Nothing -> respond $ responseLBS
                status404
                [("Content-Type", "text/plain")]
                "not found"

stringToUtf8 :: String -> BL.ByteString
stringToUtf8 = TL.encodeUtf8 . TL.pack

timeApp :: App -> Application
timeApp app rq respond = do
  _ <- logEvent app rq
  t <- getCurrentTime
  respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        (stringToUtf8 (show t))


mimicApp :: App -> Application
mimicApp app rq respond = do
  l <- logEvent app rq
  respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        (BL.fromStrict (rawBody l))


badApp :: App -> Application
badApp app rq respond = do
  _ <- logEvent app rq
  isExcept <- randomIO
  sleep    <- randomRIO (0,1::Int)
  now      <- getCurrentTime
  bracket_
    (return ())
    (return ())
    $ respond $ responseStream status200 [] $ \write flush -> do
        when isExcept $ do
          error "error"
        write $ BL.lazyByteString $ stringToUtf8 (show now ++ "\n")
        flush
        threadDelay (sleep * 1000000)
        now'      <- getCurrentTime
        -- emulating network problem
        flush
        write $ BL.lazyByteString $ stringToUtf8 (show now' ++ "\n")
        flush




main :: IO ()
main = do

  port'   <- fmap (readMaybe =<<) $ lookupEnv "PORT"
  wsPort' <- fmap (readMaybe =<<) $ lookupEnv "WEBSOCKET_PORT"
  app <- initialApp
--  _ <- forkIO $ WS.init app
--  run port (router app)

  case (port',wsPort') of
   (Just port, Just wsPort) | port /= wsPort -> do
      putStrLn $ "http://localhost:" <> show port
      _ <- forkIO $ WS.init wsPort app
      Warp.run port (router app)
   (Just port, _) -> do
      putStrLn $ "http://localhost:" <> show port
      Warp.runSettings (Warp.setPort port Warp.defaultSettings) $
        Warp.websocketsOr WS.defaultConnectionOptions (WS.application app) (router app)
   _ -> do
      putStrLn "error: add environment variable `PORT=3000 debug`"
