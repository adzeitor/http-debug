{-# LANGUAGE OverloadedStrings #-}
module View.Log where

import           Control.Monad
import           Data.CaseInsensitive          (original)
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Network.Wai
import           Network.Wai.Parse             (FileInfo (..))
import           Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Types

logToHtml :: Log -> Html
logToHtml log' = do
  let rq      = request log'
      method  = unsafeByteString (requestMethod rq)
      path    = unsafeByteString (rawPathInfo rq)
      headers = requestHeaders rq
      rawBody'= rawBody log'
      date    = show $ createdAt log'
      _id     = show $ logId log'
  h3 $ toHtml ("#" <> _id <> ": " <> date)
  h3 $ method <> " " <> path

  when (queryString rq /= []) $ do
    h5 "Query:"
    ul $ forM_ (queryString rq) $ \(k,v) -> do
      li (H.b (toHtml (unsafeByteString k))
          <> toHtml (maybe "" (\x -> ":" <> unsafeByteString x) v ))

  when (rawBody' /= "") $ do
    h5 "Raw body:"
    code ! A.style "display: block; width: 100%; overflow-x:scroll"
      $ unsafeByteString rawBody'

  case logFiles log' of
   Just list -> ul $ forM_ list $ \x -> do
     li $ do
       H.a ! A.href ( toValue ( "/file/"
                     <> show (logId log')
                     <> "/"
                     <>  (T.unpack (T.decodeUtf8 (fileName x)))
                     )) $ (unsafeByteString $ fileName x)
       H.br
       H.span (unsafeByteString $ fileContentType x)

   Nothing -> return ()

  h5 "Headers:"
  ul $ forM_ headers $ \(k,v) -> do
    li (H.b (toHtml (unsafeByteString (original k)))
             <> ":"
             <> toHtml (unsafeByteString v))
  hr

logsToHtml :: [Log] -> Html
logsToHtml logs' = do
  H.form ! A.action "/upload"
         ! A.enctype "multipart/form-data"
         ! A.method "post" $ do
     H.input ! A.type_ "file" ! A.name "file0" >> H.br
     H.input ! A.type_ "file" ! A.name "file1" >> H.br
     H.input ! A.type_ "file" ! A.name "file2" >> H.br
     H.input ! A.type_ "submit" ! A.value "Send File"

  h4 "Source code:"
  a ! A.href "https://gist.github.com/adzeitor/6d01a45f871ec364e401" $ "gist"

  h4 "Last logs:"
  H.ul $ forM_ logs' $ \x -> do
    li $ logToHtml x
