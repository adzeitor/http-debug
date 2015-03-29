module Debug where

import WebSocket as WS
import Html (..)
import Html.Lazy (..)
import Html.Attributes (..)
import Html.Attributes as Attr
import Html.Events (..)
import Json.Decode (..)
import Json.Decode as Json
import Signal
import List
import Time
import String
import Basics
import Date
import Maybe


type alias Log = { logId     : Int
                 , createdAt : String
                 , rawBody   : String
                 , method    : String
                 , rawPath   : String
                 , queryStrings : List QueryString
                 , headers      : List QueryString
                 , files        : List File
                 }



------------------- PARSING ------
type alias QueryString = {field : String
                         ,value : String}

parseQuery : Decoder QueryString
parseQuery =
    object2 QueryString
      ("field" := string)
      ("value" := string)


type alias File = {name : String
                         ,contentType : String}

parseFile : Decoder File
parseFile =
    object2 File
      ("name" := string)
      ("contentType" := string)


parseLog : Decoder Log
parseLog =
    object8 Log
      ("logId"       := int)
      ("createdAt"   := string)
      ("rawBody"     := string)
      ("method"      := string)
      ("rawPath"     := string)
      ("queryString" := Json.list parseQuery)
      ("headers"     := Json.list parseQuery)
      ("files"     := Json.list parseFile)

--------------------------------
viewQueryStrings qs =
    let go x = div [] [
                b [] [text x.field, text ": "]
               , text x.value]
    in div [] (h3 [] [text "Params"]::List.map go qs )

viewHeaders hs =
    let go x = div [] [
                b [] [text x.field, text ": "]
               , text x.value]
    in div [] (h3 [] [text "Headers"]::List.map go hs )

viewFile id x =
    let link  = "/file/" ++ toString id
                ++ "/" ++ x.name in
    if | String.startsWith "image/" x.contentType ->
      div [class "col-xs-12 col-md-6"] [
                h6 [] [text x.name]
              , a [ href link
                  , target "_blank"] [
                       img [ style [("max-width", "100%")]
                           , src link ] [ text x.name]
                      ]
              ]
       | otherwise -> div [] [
                       b [] [a [href link] [ text x.name]]
                      , text x.contentType]

viewFiles m hs =
    div [] (if List.isEmpty hs
            then []
            else h3 [] [text "Files"]::List.map (viewFile m.logId) hs )


viewRelative now time = text (Maybe.withDefault "a few seconds ago" (now `Maybe.andThen` (diffTime time)))

viewRawBody body =
    if body /= ""
    then div [] [h3 [] [text "Raw body:"]
                , pre [class "pre-scrollable"] [text body]]
    else div [] []


showLog m isNew x = div []
  [ div []
    [ h4[]
        [
          b[][text (toString x.logId)]
        , text " - "
        , b [] [viewRelative m.now x.createdAt]
        ]
      ,h4[]
         [
            b [] [text  x.method]
          , text " "
          , text x.rawPath
          , if isNew then span [class "label label-info"] [text "New"] else div [] []
         ]
    , viewRawBody x.rawBody
    , div [class "col-md-6 col-xs-12"] [viewQueryStrings x.queryStrings]
    , div [class "col-md-6 col-xs-12"] [viewHeaders x.headers]
    , div [class "col-md-12 col-xs-12"] [viewFiles x x.files]
  ]]

viewPreferenceButton m =
    button [onClick (Signal.send updates (Preference))
           , class (if m.isPreference then  "btn btn-primary active" else  "btn btn-default")]
               [ span [class "fa fa-cog"] []
               , text " preferences"
               ]

viewPreferences m =
    if m.isPreference
    then div [] [
                label [] [text "max messages: "]
              , input [
                   on "input" targetValue (Signal.send updates << UpdateMaxMessages)
                 , Attr.value (toString m.maxMessages)
                ] []
             ]
    else div [] []


viewLogo m = div[class "text-center"] [
      , span [ class "fa fa-plug", style [("font-size", "20rem")] ] []                   ]





view m = div [] [
  div [class "container text-center"]
    [ br[][]
      , h3[][text "Debug on you!!!"]
      , div [class "btn-group"]
        [
          button [onClick (Signal.send updates (Click)), class (if m.newCount > 0 then "btn btn-success" else "btn btn-default")]
            [text (if m.newCount > 0 then "show new " ++ toString m.newCount else "read all")]
          ,button [onClick (Signal.send updates (Auto)), class (if m.autoUpdate then  "btn btn-primary active" else  "btn btn-default")]
                      [ span [class "glyphicon glyphicon-refresh"] []
                      , text (if m.autoUpdate then  "Disable auto-update"  else  "Enable auto-update")
                      ]
          , viewPreferenceButton m
          , a [ href  "/tools"
              , target  "_blank"
              , class "btn btn-default"]
              [ span [class "glyphicon glyphicon-cutlery"] []
              , text " tools"
              ]
          , button [onClick (Signal.send updates (ClearOld)), class "btn btn-danger"] [text "clear old"]
        ]
    ]
  , viewPreferences m
  ,div[class "container-fluid"]
    [
        div []
        (List.map (showLog m True ) m.new)
      , div []
        (List.map (showLog m False) m.old)
    ]
  , viewLogo m
  ]


-----------------------------------------------


type alias Model = { new : List Log
                   , old : List Log
                   , pool : List Log
                   , newCount : Int
                   , autoUpdate : Bool
                   , now : Maybe Time.Time
                   , isPreference : Bool
                   , maxMessages : Int}

initial =  { new = []
           , old = []
           , pool = []
           , newCount = 0
           , autoUpdate = True
           , now = Nothing
           , isPreference = False
           , maxMessages = 128}


type Action = Sockets (Result String Log)
            | Timed Time.Time
            | ReadAll
            | ClearOld
            | Auto
            | Click
            | NoOp
            | Preference
            | UpdateMaxMessages String


updates : Signal.Channel Action
updates = Signal.channel NoOp

noOut = Signal.sampleOn (Signal.constant False)
     <| Signal.constant ""

server =
    let url = "ws://" ++ getLocation ++ ":9160"
    in Signal.map (Sockets << decodeString parseLog)
    <| WS.connect wsurl noOut


port getLocation : String


wsurl = "ws://localhost:9160"


update : Action -> Model -> Model
update s m = case s of
  (Sockets (Ok val)) ->
    if m.autoUpdate
    then {m | new      <- List.take m.maxMessages (val::m.new)
            , old      <- m.old
            }
    else {m | pool     <- List.take m.maxMessages (val::m.pool)
         , newCount <- Basics.min m.maxMessages (m.newCount + 1)}
  Click -> { m | new      <- m.pool
               , pool <- []
               , old      <- List.take m.maxMessages (m.new ++ m.old)
               , newCount <- 0 }
  Auto -> update Click { m | autoUpdate <- not m.autoUpdate}
  Preference -> { m | isPreference <- not m.isPreference}
  ClearOld -> if m.autoUpdate
              then {m | old <- m.new
                      , new <- []}
              else {m | old <- []
                      , new <- []}
  ReadAll ->
      if m.autoUpdate
      then {m | old <- List.take m.maxMessages (m.new ++ m.old)
              , new <- []
           }
      else m
  Timed t -> {m | now <- Just t}
  UpdateMaxMessages str ->
      case String.toInt str of
        Ok a  -> { m | maxMessages <- a}
        _     -> m

  _    -> m

signals = Signal.mergeMany
   [ server
   , Signal.subscribe updates
   , Signal.map (\_ -> ReadAll) <| Time.every (60*Time.second)
   , Signal.map Timed <| Time.every (1*Time.second)]


main = Signal.map (lazy view) (Signal.foldp update initial signals)


----------------------- TIME ---------------------


formatRelative : Time.Time -> String
formatRelative d' =
  let d      = d'/1000
      minute = 60
      hour   = minute*60
      day    = hour*24
  in if | d < minute   -> "a " ++ toString (round d % 60) ++ " seconds ago"
        | d < hour      ->
             toString (round (d/minute )) ++ " minutes ago"
        | d < day ->
             toString (round (d/hour )) ++ " hours ago"
        | otherwise -> "TODO"

diffTime : String -> Time.Time -> Maybe String
diffTime timeStr now =
  case Date.fromString timeStr of
    Ok t -> let diff = (now - Date.toTime t)
            in Just (formatRelative diff)
    _    -> Nothing