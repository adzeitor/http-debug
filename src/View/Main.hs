{-# LANGUAGE OverloadedStrings #-}
module View.Main where

import           Data.Monoid
import qualified Data.Set                      as Set
import           Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

import           Types
import           View.Log


links :: Html
links = do
  h4 "Links:"
  H.ul $ mapM_ (\x -> li (a ! A.href (textValue x) $ toHtml x) )
    [ "/time"
    , "/bad"
    , "/mimic"
    , "/upload"
    , "/auth"
    ]


mainPage :: [Log]
         -> Hits
         -> UniqHosts
         -> Html
mainPage logs' hits' uniqHosts = do
  H.html $ do
    H.head $ do
      H.title "logs"
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
    H.body $ do
      links
      h5 $ b "Hits: " <>  toHtml (show hits')
      h5 $ b "Hosts: " <> toHtml (show (Set.size uniqHosts))
      logsToHtml logs'
