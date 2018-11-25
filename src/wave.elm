port module Main exposing (..)

{- TODO
   nice icons

   wave moving across the screen
-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck, onBlur, keyCode, targetValue, on)
import Html.Lazy
import Task
import Time

import File.Download

import Json.Encode
import Json.Decode exposing (field)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, end, oneOf)

-- PORTS

port saveSettings : Json.Encode.Value -> Cmd msg

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- HELPERS

timeDifference : Time.Posix -> Time.Posix -> Int
timeDifference timeNow timeThen =
    let nowFloored  = Time.posixToMillis timeNow // 1000
        thenFloored = (Time.posixToMillis timeThen // 1000)
    in 1000 * abs (nowFloored - thenFloored)

-- MODEL
                     
type State = Loading
           | Quiescent
           | BetweenWaves
           | Waving
           | GracePeriod Time.Posix -- timeEntered to restore

type LogEntry = Began
              | Ended
              | CryingStarted
              | CryingStopped
              | CryingSquashed
              | Waved Time.Posix -- time was due
              | Debug String

type RemainingMode = ClockAndTimeLeft
                   | ClockOnly
                   | TimeLeftOnly

type alias TimeLeft =
    { remaining : Int
    , targetTime : Time.Posix
    }

waveDue : Model -> TimeLeft -> Bool
waveDue model { targetTime } =
    Time.posixToMillis model.time >= Time.posixToMillis targetTime
    
nextRemainingMode : RemainingMode -> RemainingMode
nextRemainingMode mode =
    case mode of
        ClockAndTimeLeft -> ClockOnly
        ClockOnly -> TimeLeftOnly
        TimeLeftOnly -> ClockAndTimeLeft
                     
isWaved : (Time.Posix,LogEntry) -> Bool
isWaved (_,entry) =
    case entry of
        Waved _ -> True
        _ -> False
            
countWaves : Model -> Int
countWaves model = List.filter isWaved model.log |> List.length
                
type alias Log = List (Time.Posix, LogEntry)

-- the persistent parts, a subtype of Config
type alias Settings =
  { waveTime : Int -- millis
  , graceTime : Int -- millis
  , twelveHour : Bool
  , remainingMode : RemainingMode
  }

defaultSettings =
    { waveTime = 1000 * 60 * 5 -- 5min wave time
    , graceTime = 1000 * 30 -- 30sec grace period
    , twelveHour = True
    , remainingMode = ClockAndTimeLeft
    }
    
type alias Config =
  { zone : Time.Zone -- dynamically loaded every time
  , waveTime : Int -- millis
  , graceTime : Int -- millis
  , twelveHour : Bool
  , remainingMode : RemainingMode

  -- for updates
  , enteredWaveTime : String
  , enteredGraceTime : String
  }

saveConfig : Config -> Cmd msg
saveConfig config =
    settingsEncoder
        { waveTime = config.waveTime
        , graceTime = config.graceTime
        , twelveHour = config.twelveHour
        , remainingMode = config.remainingMode
        } 
        |> saveSettings

setZone : Time.Zone -> Config -> Config
setZone newZone config = { config | zone = newZone }

setTwelveHour : Bool -> Config -> Config
setTwelveHour newTwelveHour config = { config | twelveHour = newTwelveHour }
                                     
updateWaveTime : String -> Config -> Config
updateWaveTime entered config = { config | enteredWaveTime = entered }
                             
updateGraceTime : String -> Config -> Config
updateGraceTime entered config = { config | enteredGraceTime = entered }

cycleRemainingMode : Config -> Config
cycleRemainingMode config =
    { config | remainingMode = nextRemainingMode config.remainingMode }

settingsDecoder : Json.Decode.Decoder Settings
settingsDecoder =
    Json.Decode.map4
        (\waveTime graceTime twelveHour remainingMode ->
             { waveTime = waveTime
             , graceTime = graceTime
             , twelveHour = twelveHour
             , remainingMode = remainingMode
             })
        (field "waveTime" Json.Decode.int)
        (field "graceTime" Json.Decode.int)
        (field "twelveHour" Json.Decode.bool)
        (field "remainingMode" remainingModeDecoder)

settingsEncoder : Settings -> Json.Encode.Value
settingsEncoder settings =
    Json.Encode.object
        [ ("waveTime", Json.Encode.int settings.waveTime)
        , ("graceTime", Json.Encode.int settings.graceTime)
        , ("twelveHour", Json.Encode.bool settings.twelveHour)
        , ("remainingMode", remainingModeEncoder settings.remainingMode)
        ]
            
remainingModeDecoder : Json.Decode.Decoder RemainingMode
remainingModeDecoder =
    Json.Decode.string |> Json.Decode.andThen
        (\s -> case s of
                   "ClockAndTimeLeft" -> Json.Decode.succeed ClockAndTimeLeft
                   "ClockOnly"        -> Json.Decode.succeed ClockOnly
                   "TimeLeftOnly"     -> Json.Decode.succeed TimeLeftOnly
                   _ -> Json.Decode.fail "bad remaining mode")

remainingModeEncoder : RemainingMode -> Json.Encode.Value
remainingModeEncoder mode =
    case mode of
        ClockAndTimeLeft -> Json.Encode.string "ClockAndTimeLeft"
        ClockOnly        -> Json.Encode.string "ClockOnly"
        TimeLeftOnly     -> Json.Encode.string "TimeLeftOnly"
            
type alias Model = 
  { state : State
  , time : Time.Posix
  , timeBegun : Time.Posix
  , priorMillis : Int
  , timeEntered : Time.Posix
  , log : Log
  , config : Config
  }

logEntry : LogEntry -> Model -> Model
logEntry entry model = { model | log = (model.time,entry)::model.log }

squashCryingStopped : Model -> Model
squashCryingStopped model =
    case model.log of
        (time, CryingStopped) :: oldLog ->
            { model | log = (time, CryingSquashed):: oldLog }
        _ -> model

hasBegun : Model -> Bool
hasBegun model = model.timeBegun /= Time.millisToPosix 0

initConfig : Settings -> Config
initConfig settings =
    { zone = Time.utc
    , waveTime = settings.waveTime
    , graceTime = settings.graceTime
    , twelveHour = settings.twelveHour
    , remainingMode = settings.remainingMode
    , enteredWaveTime = millisToHMSShort settings.waveTime
    , enteredGraceTime = millisToHMSShort settings.graceTime
    }
    
init : Json.Encode.Value -> (Model, Cmd Msg)
init localStorageSettings =
  ( { state = Loading
    , time = Time.millisToPosix 0
    , timeBegun = Time.millisToPosix 0
    , priorMillis = 0
    , timeEntered = Time.millisToPosix 0
    , log = []
    , config =
        initConfig
        (case Json.Decode.decodeValue settingsDecoder localStorageSettings of
             Err _ -> defaultSettings
             Ok settings -> settings)
    }
  , Task.perform (\x -> x) (Task.map2 InitializeTime Time.here Time.now)
  )


-- UPDATE


type Msg
  = Tick Time.Posix
  | InitializeTime Time.Zone Time.Posix
  | Begin
  | End
  | StartWave
  | EndWave
  | ResumeWave Time.Posix
  | Wave
  | DownloadLog
  | ConfigUpdateWaveTime String
  | ConfigUpdateGraceTime String
  | ConfigSetWaveTime
  | ConfigSetGraceTime
  | ConfigSetTwelveHour Bool
  | ConfigCycleRemaining

checkTimers : Model -> (Model, Cmd Msg)
checkTimers model =
    case model.state of
        -- no interesting timers running
        Loading -> (model, Cmd.none)
        Quiescent -> (model, Cmd.none)
        Waving -> (model, Cmd.none)

        BetweenWaves ->
            if timeDifference model.time model.timeEntered >= model.config.waveTime
            then -- time for a wave
                ({ model | state = Waving,
                           timeEntered = model.time }
                , Cmd.none)
            else (model, Cmd.none)
                
        GracePeriod originalTime ->  
            if timeDifference model.time model.timeEntered >= model.config.graceTime
            then -- great, no need for a wave
                ({ model | state = Quiescent,
                           timeEntered = model.time
                 }
                , Cmd.none)
            else (model, Cmd.none)

updateAndSaveConfig : Config -> Model -> (Model, Cmd Msg)
updateAndSaveConfig newConfig model =       
    ( { model | config = newConfig }, saveConfig newConfig)
            
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime -> checkTimers { model | time = newTime }

    InitializeTime newZone newTime ->
      ( { model | time = newTime,
                  config = setZone newZone model.config }
      , Cmd.none)

    Begin ->
      ( logEntry Began
            { model | state = Quiescent,
                      timeEntered = model.time,
                      timeBegun = model.time }
      , Cmd.none)

    End ->
      ( logEntry Ended
            { model | state = Loading,
                      timeEntered = model.time,
                      timeBegun = Time.millisToPosix 0,
                      priorMillis = model.priorMillis +
                                    timeDifference model.time model.timeBegun }
      , Cmd.none)
        
    StartWave ->
      ( logEntry CryingStarted
            { model | state = BetweenWaves,
                      timeEntered = model.time }
      , Cmd.none)

    EndWave ->
      ( logEntry CryingStopped
            { model | state = GracePeriod model.timeEntered,
                      timeEntered = model.time }
      , Cmd.none)

    ResumeWave originalTimeEntered ->
      ( squashCryingStopped
            { model | state = BetweenWaves,
                      timeEntered = originalTimeEntered }
      , Cmd.none)

    Wave ->
      ( logEntry (Waved model.timeEntered)
            { model | state = BetweenWaves,
                      timeEntered = model.time }
      , Cmd.none)

    DownloadLog ->
      ( model
      , let now = clockTime model.config model.time in
        let filename = "sleep-" ++ now ++ ".csv" in
        File.Download.string filename "text/csv" (logToCSV model.config model.log))
       
    ConfigUpdateWaveTime newWaveTime ->
      ( { model | config = updateWaveTime newWaveTime model.config }
      , Cmd.none)
        
    ConfigUpdateGraceTime newGraceTime ->
      ( { model | config = updateGraceTime newGraceTime model.config }
      , Cmd.none)
       
    ConfigSetWaveTime ->
        let config = model.config in
        case tryHMSToMillis config.enteredWaveTime of
            Nothing -> (model, Cmd.none)
            Just new -> 
                updateAndSaveConfig 
                  { config |
                        waveTime = new,
                        enteredWaveTime = millisToHMSShort new }
                  model

    ConfigSetGraceTime ->
        let config = model.config in
        case tryHMSToMillis config.enteredGraceTime of
            Nothing -> (model, Cmd.none)
            Just new -> 
                updateAndSaveConfig 
                  { config |
                        graceTime = new,
                        enteredGraceTime = millisToHMSShort new }
                  model

    ConfigSetTwelveHour newTwelveHour ->
      updateAndSaveConfig (setTwelveHour newTwelveHour model.config) model

    ConfigCycleRemaining ->
      updateAndSaveConfig (cycleRemainingMode model.config) model

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 250 Tick


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ row "clock" <| viewClock model
    , row "remaining" <| viewRemaining model
    , row "actions"  <| viewActions model
    , row "info" <| viewInfo model
    , Html.Lazy.lazy2 viewLog model.config model.log
    , Html.Lazy.lazy (viewConfig >> row "config") model.config
    ]

viewClock : Model -> Html msg
viewClock model =
    h1 [ class "clock"
       , centered ]
       [ text (clockTime model.config model.time) ]
            
viewRemaining : Model -> Html Msg
viewRemaining model =
    div [ class "ten columns offset-by-one" ]
        (case model.state of
             Loading -> []
             Quiescent -> []
             BetweenWaves ->
                 let computed =
                         computeTimeLeft model model.timeEntered
                             model.config.waveTime
                 in
                 [ div [ class "next-wave" ]
                       [ remainingTime model computed ]
                 ]
             Waving ->
                 [ h1 [ class "wave" ]
                      [ text "time for a visit" ]
                 ]
             GracePeriod originalTimeEntered ->
                 let computed =
                         computeTimeLeft model originalTimeEntered
                             model.config.waveTime
                 in
               [ div [ class "resume" ]
                     [ remainingTime model computed
                     ]
               , div [ class "grace" ]
                     [ timeLeft (computeTimeLeft model
                                     model.timeEntered model.config.graceTime)
                           "left in grace period"
                     ]
               ]
      )

remainingTime : Model -> TimeLeft -> Html Msg
remainingTime model computed =
    let clock = targetTimeClock model.config computed
        lbl   = span [] [ text "next wave" ]
        left  = timeLeft computed ""
    in
        div [ class "remaining"
            , onClick ConfigCycleRemaining
            ]
            (if waveDue model computed
             then [ h1 [ class "wave" ]
                        [ text "be ready for a visit" ]
                  ]
             else case model.config.remainingMode of
                      ClockAndTimeLeft -> [ lbl, clock, left ]
                      ClockOnly        -> [ lbl, clock ]
                      TimeLeftOnly     -> [ lbl,        left ]
            )

timeLeft : TimeLeft -> String -> Html msg
timeLeft { remaining, targetTime } msg =
    h4 [class "timeleft"]
        [ text ("" ++ millisToHMSShort remaining ++ " " ++ msg)
        ]
        
targetTimeClock : Config -> TimeLeft -> Html msg
targetTimeClock config { remaining, targetTime } =
    h1 [class "target"] [ text (clockTime config targetTime) ]
        
computeTimeLeft : Model -> Time.Posix -> Int -> TimeLeft
computeTimeLeft model timeStarted duration =
    let elapsed    = timeDifference model.time timeStarted
        remaining  = duration - elapsed
        targetTime = Time.posixToMillis model.time + remaining |> Time.millisToPosix
    in
        { remaining = remaining
        , targetTime = targetTime
        }
        
viewActions : Model -> Html Msg
viewActions model =
    let actionClass handler = [ centered, handler ] in
    case model.state of
        Loading ->
            button (actionClass (onClick Begin)) [ text "begin" ]
             
        Quiescent ->
            div []
                [ button (actionClass (onClick StartWave)) [ text "crying started" ]
                , button (actionClass (onClick End)) [ text "end" ]
                ]
        
        BetweenWaves ->
            button (actionClass (onClick EndWave)) [ text "crying stopped" ]
        
        Waving ->
            button (actionClass (onClick Wave)) [ text "i waved" ]

        GracePeriod originalTimeEntered ->
            button (actionClass (onClick (ResumeWave originalTimeEntered)))
                [ text "crying restarted" ]
                    
viewInfo : Model -> Html msg
viewInfo model =
    div [ ]
        (if hasBegun model
         then [ rowLabel "Information"
              , div [ class "duration two columns offset-by-two" ]
                    [ text "Total time: "
                    , text (millisToHMSLong
                                (timeDifference model.time model.timeBegun +
                                 model.priorMillis))
                    ]
              , Html.Lazy.lazy viewWaveCount (countWaves model)
              ]
         else [ ])

viewWaveCount : Int -> Html msg
viewWaveCount numWaves =
    div [ class "waves two columns" ]
        [ if numWaves == 0
          then text "0 waves"
          else text (countPlural numWaves "wave") ]

viewLog : Config -> Log -> Html Msg
viewLog config log =
    div []
        [ row "log" <|
              table [ class "log ten columns offset-by-one" ]
              (if List.isEmpty log
               then []
               else
                   [ thead []
                         [ tr []
                               [ td [] [ text "Time" ]
                               , td [] [ text "Event" ] ]
                         ]
                   , tbody [] (List.map (viewLogEntry config) log)
                   ]
              )
        , row "log-download" <|
            if List.isEmpty log
            then span [] []
            else button [ class "three columns offset-by-eight"
                        , onClick DownloadLog
                        ]
                    [ text "download log " ]
        ]
            

            
viewLogEntry : Config -> (Time.Posix, LogEntry) -> Html msg
viewLogEntry config (time, entry) =
    tr [ class "entry" ]
        [ td [ class "time" ] [ text (clockTime config time) ]
        , td [ class "event" ]
             ( case entry of
                   Began -> [ text "began" ]
                   Ended -> [ text "ended" ]
                   CryingStarted -> [ text "crying started" ]
                   CryingStopped -> [ text "crying stopped" ]
                   CryingSquashed -> [ s [] [ text "crying stopped" ] ]
                   Waved timeDue ->
                       [ text "completed wave due at "
                       , text (clockTime config timeDue)
                       ]
                   Debug s -> [ text s ]
             )
        ]

viewConfig : Config -> Html Msg
viewConfig config =
    div [ ]
        [ rowLabel "Settings"
        , div [ class "two columns offset-by-one" ]
            (timeInput config .enteredWaveTime .waveTime
                 "config-wave-duration" "Wave duration (H:M:S)"
                 ConfigUpdateWaveTime ConfigSetWaveTime)
        , div [ class "two columns" ]
            (timeInput config .enteredGraceTime .graceTime
                 "config-grace-time" "Grace period (H:M:S)"
                 ConfigUpdateGraceTime ConfigSetGraceTime)
        , div [ class "three columns", id "config-twelve-hour" ]
            [ div []
                  [ input [ id "config-twelve-hour12"
                          , name "config-twelve-hour"
                          , type_ "radio"
                          , checked config.twelveHour
                          , onCheck ConfigSetTwelveHour
                          ]
                        []
                  , label [ for "config-twelve-hour12" ]
                      [ text "12-hour clock (am/pm)" ]
                  ]
            , div []
                  [ input [ id "config-twelve-hour24"
                          , name "config-twelve-hour"
                          , type_ "radio"
                          , checked (not config.twelveHour)
                          , onCheck (not >> ConfigSetTwelveHour)
                          ]
                        []
                  , label [ for "config-twelve-hour24" ]
                      [ text "24-hour clock" ]
                  ]
            ]
        ]

timeInput : Config -> (Config -> String) -> (Config -> Int) -> String -> String -> (String -> Msg) -> Msg -> List (Html Msg)
timeInput config entered actual inputId labelText updateMsg setMsg =
    let inSync = (entered config) == millisToHMSShort (actual config) in
    let valid = isValidHMS (entered config) in
    [ label [ for inputId ] [ text (labelText ++ " ") ]
    , input [ type_ "text"
            , value (entered config)
            , onInput updateMsg
            , onEnter setMsg
            , onBlur setMsg
            , id inputId
            , class (if valid
                     then if inSync
                          then "valid"
                          else "outofdate"
                     else "invalid")
            ]
          []
    ]
        
onEnter : msg -> Attribute msg
onEnter msg =
  let isEnter code = if code == 13
                     then Json.Decode.succeed ""
                     else Json.Decode.fail ""
      decodeEnter = Json.Decode.andThen isEnter keyCode
  in
      on "keypress" (Json.Decode.map (\key -> msg) decodeEnter)
        
clockTime : Config -> Time.Posix -> String
clockTime config time = 
  let baseHour  = Time.toHour config.zone time
      hour      = formatHour config baseHour
      minute    = twoDigitInt    (Time.toMinute config.zone time)
      second    = twoDigitInt    (Time.toSecond config.zone time)
      ampm      = if config.twelveHour
                  then if baseHour >= 12
                       then "pm"
                       else "am"
                  else ""
  in
  hour ++ ":" ++ minute ++ ":" ++ second ++ ampm

formatHour : Config -> Int -> String
formatHour config baseHour =
    let hourNum = if config.twelveHour
                  then modBy 12 baseHour
                  else baseHour
        hour12  = if hourNum == 0 then 12 else hourNum
    in
        String.fromInt hour12
      
millisToLongTime : Int -> String
millisToLongTime millis =
    let (hours, minutes, seconds) = millisToHMS millis in
    let strs = [ countPlural hours "hour"
               , countPlural minutes "minute"
               , countPlural seconds "second"
               ]
    in
    List.filter (not << String.isEmpty) strs |> String.join " "
        
millisToHMSShort : Int -> String
millisToHMSShort millis =
    let (hours, minutes, seconds) = millisToHMS millis in
    let strs = [ if hours > 0 then String.fromInt hours ++ ":" else ""
               , if hours > 0
                 then twoDigitInt minutes
                 else if minutes > 0
                      then String.fromInt minutes
                      else ""
               , if hours > 0 || minutes > 0
                 then twoDigitInt seconds
                 else String.fromInt seconds
               ]
    in
    List.filter (not << String.isEmpty) strs |> String.join ":"

millisToHMSLong : Int -> String
millisToHMSLong millis =
    let (hours, minutes, seconds) = millisToHMS millis in
    (if hours > 0 then String.fromInt hours ++ ":" else "") ++
    twoDigitInt minutes ++ ":" ++
    twoDigitInt seconds
        
millisToHMS : Int -> (Int, Int, Int)
millisToHMS millis =              
    let totalSeconds = millis // 1000 in
    let seconds = modBy 60 totalSeconds in
    let totalMinutes = totalSeconds // 60 in
    let minutes = modBy 60 totalMinutes in
    let hours = totalMinutes // 60 in
    (hours, minutes, seconds)

twoDigitInt : Int -> String
twoDigitInt n = String.padLeft 2 '0' (String.fromInt n)

countPlural : Int -> String -> String
countPlural n thing =
    case n of
        0 -> ""
        1 -> "1 " ++ thing
        _ -> String.fromInt n ++ " " ++ thing ++ "s"

-- skeleton wrapper

row : String -> Html msg -> Html msg
row cls cts = div [ class "row", class cls ] [ cts ]

rowLabel : String -> Html msg
rowLabel lbl = h5 [ class "one column"] [ text lbl ]
              
centered : Attribute msg
centered = class "six columns offset-by-three"

-- H:M:S parser             

tryHMSToMillis : String -> Maybe Int
tryHMSToMillis str =
    case Parser.run parseHMS str of
        Err _ -> Nothing
        Ok (h,m,s) -> Just (1000 * (s + 60 * (m + 60 * h)))

isValidHMS : String -> Bool
isValidHMS str =
    case Parser.run parseHMS str of
        Err _ -> False
        Ok (h,m,s) -> True
                      
parseHMS : Parser (Int,Int,Int)
parseHMS =
    succeed (\i1 mi23 ->
                 case mi23 of
                     Nothing -> (0, 0, i1)
                     Just (i2, Nothing) -> (0, i1, i2)
                     Just (i2, Just i3) -> (i1, i2, i3))
    |= relaxedInt
    |= oneOf [succeed (\i2 mi3 -> Just (i2,mi3))
             |. symbol ":"
             |= relaxedInt
             |= oneOf [succeed Just
                      |. symbol ":"
                      |= relaxedInt
                      , succeed Nothing
                      ]
             , succeed Nothing
             ]
    |. end


relaxedInt : Parser Int
relaxedInt =
    Parser.map (\str ->
                    case String.toInt str of
                        Nothing -> 0
                        Just n -> n)
        <| Parser.getChompedString <| Parser.chompWhile Char.isDigit

-- log CSV renderer
logToCSV : Config -> Log -> String
logToCSV config log =
    String.join "\n" (List.filterMap (logRowToCSV config) log) ++ "\n"

logRowToCSV : Config -> (Time.Posix, LogEntry) -> Maybe String
logRowToCSV config (time, entry) =
    logEntryToString config entry
        |> Maybe.andThen (\e -> Just (clockTime config time ++ "," ++ e))

logEntryToString : Config -> LogEntry -> Maybe String
logEntryToString config entry =
    case entry of
        Began -> Just "began"
        Ended -> Just "ended"
        CryingStarted -> Just "crying started"
        CryingStopped -> Just "crying stopped"
        CryingSquashed -> Nothing
        Waved timeDue -> Just ("completed wave due at " ++ clockTime config timeDue)
        Debug s -> Nothing