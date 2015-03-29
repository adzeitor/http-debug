{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import           Control.Applicative
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.CaseInsensitive   (CI, original)
import           Data.IORef
import           Data.Maybe             (fromMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import           Data.Time
import qualified Data.Vector            as V
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Parse      (FileInfo (..))
import           System.Environment     (lookupEnv)
import           Text.Read              (readMaybe)

type MyFileInfo = FileInfo BL.ByteString


data Log = Log { request   :: !Request
               , rawBody   :: !BS.ByteString
               , createdAt :: !UTCTime
               , logId     :: !Integer
               , logFiles  :: Maybe [MyFileInfo]
               }
           deriving (Show, Generic)

data Query = Query { field :: Text
                   , value :: Text }
             deriving (Show,Generic)


data File = File { name        :: Text
                 , contentType :: Text }
          deriving (Show,Generic)


instance ToJSON Query
instance ToJSON File



toQuery :: (BS.ByteString, Maybe BS.ByteString) -> Query
toQuery (k,v) = Query (T.decodeUtf8 k) (maybe "" T.decodeUtf8 v)

toHeaders :: (CI BS.ByteString,BS.ByteString) -> Query
toHeaders (k,v) = Query (T.decodeUtf8 (original k)) (T.decodeUtf8 v)


toFile :: MyFileInfo -> File
toFile f = File (T.decodeUtf8 (fileName f)) (T.decodeUtf8 (fileContentType f))

instance ToJSON Log where
  toJSON x = object [ "createdAt" .= toJSON (createdAt x)
                    , "logId"     .= toJSON (logId x)
                    , "rawBody"   .= toJSON (T.decodeUtf8 (rawBody x))
                    , "method"    .= toJSON (T.decodeUtf8 (requestMethod (request x)))
                    , "rawPath"   .= toJSON (T.decodeUtf8 (rawPathInfo (request x)))
                    , "queryString" .=  toJSON (map toQuery (queryString (request x)))
                    , "headers" .=  toJSON (map toHeaders (requestHeaders (request x)))
                    , "files" .=  toJSON (maybe [] (map toFile) (logFiles x))
                    ]


type UniqHosts = Set String
type Hits      = Int


data App = App { hits       :: IORef Hits
               , hosts      :: IORef UniqHosts
               , logs       :: IORef (V.Vector Log)
               , nextLogId  :: IORef Integer
               , maxLogSize :: Int
               , logChan    :: TChan Log
               }

initialApp :: IO App
initialApp  = do
  hitsRef  <- newIORef 0
  hostsRef <- newIORef (Set.empty)
  logsRef  <- newIORef V.empty
  logChan' <- atomically $ newBroadcastTChan
  nextLogIdRef <- newIORef 0
  logSize <- (readMaybe =<<) <$> (lookupEnv "MAX_LOG_SIZE" )
  return (App { hits  = hitsRef
              , hosts = hostsRef
              , logs  = logsRef
              , nextLogId = nextLogIdRef
              , maxLogSize = fromMaybe 128 logSize
              , logChan = logChan'})
