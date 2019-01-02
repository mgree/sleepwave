port module Main exposing (..)

{- TODO
   clarify state semantics---docs at the bottom?

   'delay wave' with grace time?

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
port copyToClipboard : String -> Cmd msg
port notify : String -> Cmd msg
port notificationPermission : (Json.Encode.Value -> msg) -> Sub msg

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
  , notifyOnWave : Bool
  , remainingMode : RemainingMode
  }

defaultSettings =
    { waveTime = 1000 * 60 * 5 -- 5min wave time
    , graceTime = 1000 * 30 -- 30sec grace period
    , twelveHour = True
    , notifyOnWave = True
    , remainingMode = ClockAndTimeLeft
    }
    
type alias Config =
  { zone : Time.Zone -- dynamically loaded every time
  , waveTime : Int -- millis
  , graceTime : Int -- millis
  , twelveHour : Bool
  , notifyOnWave : Bool
  , remainingMode : RemainingMode

  -- for notifications
  , notificationsPermitted : Bool

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
        , notifyOnWave = config.notifyOnWave
        , remainingMode = config.remainingMode
        } 
        |> saveSettings

setZone : Time.Zone -> Config -> Config
setZone newZone config = { config | zone = newZone }

setTwelveHour : Bool -> Config -> Config
setTwelveHour newTwelveHour config = { config | twelveHour = newTwelveHour }
                             
setNotifyOnWave : Bool -> Config -> Config
setNotifyOnWave newNotifyOnWave config = { config | notifyOnWave = newNotifyOnWave }
        
updateWaveTime : String -> Config -> Config
updateWaveTime entered config = { config | enteredWaveTime = entered }
                             
updateGraceTime : String -> Config -> Config
updateGraceTime entered config = { config | enteredGraceTime = entered }

updateNotificationPermission : Bool -> Config -> Config
updateNotificationPermission perm config = 
    { config | notificationsPermitted = perm }


cycleRemainingMode : Config -> Config
cycleRemainingMode config =
    { config | remainingMode = nextRemainingMode config.remainingMode }

settingsDecoder : Json.Decode.Decoder Settings
settingsDecoder =
    Json.Decode.map5
        (\waveTime graceTime twelveHour notifyOnWave remainingMode ->
             { waveTime = waveTime
             , graceTime = graceTime
             , twelveHour = twelveHour
             , notifyOnWave = notifyOnWave
             , remainingMode = remainingMode
             })
        (field "waveTime" Json.Decode.int)
        (field "graceTime" Json.Decode.int)
        (field "twelveHour" Json.Decode.bool)
        (field "notifyOnWave" Json.Decode.bool)
        (field "remainingMode" remainingModeDecoder)

settingsEncoder : Settings -> Json.Encode.Value
settingsEncoder settings =
    Json.Encode.object
        [ ("waveTime", Json.Encode.int settings.waveTime)
        , ("graceTime", Json.Encode.int settings.graceTime)
        , ("twelveHour", Json.Encode.bool settings.twelveHour)
        , ("notifyOnWave", Json.Encode.bool settings.notifyOnWave)
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
    , notifyOnWave = settings.notifyOnWave
    , remainingMode = settings.remainingMode
    , notificationsPermitted = False
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
  | CopyLog
  | DownloadLog
  | ConfigUpdateWaveTime String
  | ConfigUpdateGraceTime String
  | ConfigSetWaveTime
  | ConfigSetGraceTime
  | ConfigSetTwelveHour Bool
  | ConfigSetNotifyOnWave Bool
  | ConfigNotificationPermission Bool
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
                , if model.config.notifyOnWave
                  then notify (clockTime model.config model.timeEntered ++ " -- Time for a visit!")
                  else Cmd.none)
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

    CopyLog ->
      ( model
      , copyToClipboard logBodyID)

    DownloadLog ->
      ( model
      , let dateNow = date model.config model.time in
        let timeNow = clockTime model.config model.time in
        let filename = "sleep-" ++ dateNow ++ "-" ++ timeNow ++ ".csv" in
        File.Download.string filename "text/csv" (logToCSV model.config model.log))
       
    ConfigUpdateWaveTime newWaveTime ->
      ( { model | config = updateWaveTime newWaveTime model.config }
      , Cmd.none)
        
    ConfigUpdateGraceTime newGraceTime ->
      ( { model | config = updateGraceTime newGraceTime model.config }
      , Cmd.none)

    ConfigNotificationPermission newNotificationPermission ->
      ( { model | config = updateNotificationPermission newNotificationPermission model.config }
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

    ConfigSetNotifyOnWave newNotifyOnWave ->
      updateAndSaveConfig (setNotifyOnWave newNotifyOnWave model.config) model

    ConfigCycleRemaining ->
      updateAndSaveConfig (cycleRemainingMode model.config) model

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch [ Time.every 250 Tick
              , notificationPermission 
                    (\json -> 
                         ConfigNotificationPermission
                           (Result.withDefault False 
                                (Json.Decode.decodeValue Json.Decode.bool json)))
              ]


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ row "clock" <| viewClock model
    , row "actions"  <| viewActions model
    , row "remaining" <| viewRemaining model
    , viewInfo model
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
                 let computed = computeTimeLeft model model.timeEntered
                                  model.config.waveTime
                 in
                 [ remainingTime model computed ]
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
    let lbl   = span [] [ icon "far fa-hand-paper"
                        , text " next wave" ]
        left  = timeLeft computed ""
        clock = targetTimeClock model.config computed

    in
        div [ class "next-wave"
            , onClick ConfigCycleRemaining
            ]
            (if waveDue model computed
             then [ h1 [ class "wave" ]
                        [ text "be ready for a visit" ]
                  ]
             else case model.config.remainingMode of
                      ClockAndTimeLeft -> [ lbl, left, clock ]
                      ClockOnly        -> [ lbl,       clock ]
                      TimeLeftOnly     -> [ lbl, left ]
            )

timeLeft : TimeLeft -> String -> Html msg
timeLeft { remaining, targetTime } msg =
    h1 [class "timeleft"]
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
            button (actionClass (onClick Begin)) 
                [ icon "far fa-play-circle"
                , text " begin" ]
             
        Quiescent ->
            div []
                [ button (actionClass (onClick StartWave)) 
                      [ icon "far fa-tired"
                      , text " crying started" ]
                , button (actionClass (onClick End)) 
                    [ icon "far fa-stop-circle"
                    , text " end" ]
                ]
        
        BetweenWaves ->
            button (actionClass (onClick EndWave)) 
                [ icon "far fa-smile-beam"
                , text " crying stopped" ]
        
        Waving ->
            button (actionClass (onClick Wave)) 
                [ icon "far fa-hand-paper"
                , text " i waved" ]

        GracePeriod originalTimeEntered ->
            button (actionClass (onClick (ResumeWave originalTimeEntered)))
                [ icon "far fa-tired"
                , text " crying restarted" ]
                    
viewInfo : Model -> Html msg
viewInfo model =
    rowCts "info"
        (if hasBegun model
         then [ rowLabel "Information"
              , div [ class "duration four columns offset-by-two" ]
                    [ text "Total time monitoring: "
                    , text (millisToHMSLong
                                (timeDifference model.time model.timeBegun +
                                 model.priorMillis))
                    ]
              , Html.Lazy.lazy viewWaveCount (countWaves model)
              ]
         else [ ])

viewWaveCount : Int -> Html msg
viewWaveCount numWaves =
    div [ class "waves three columns" ]
        [ icon "far fa-hand-paper"
        , text " "
        , if numWaves == 0
          then text "0 waves"
          else text (countPlural numWaves "wave") ]

logBodyID : String
logBodyID = "log-body"

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
                   , tbody [ id logBodyID ] 
                       (List.map (viewLogEntry config) log)
                   ]
              )
        , rowCts "log-download" <|
            if List.isEmpty log
            then []
            else [ div [ class "four columns offset-by-seven" ]
                       [ button [ onClick CopyLog ]
                           [ icon "far fa-copy"
                           , text " copy log to clipboard" ]
                       , button [ onClick DownloadLog ]
                           [ icon "fas fa-download"
                           , text " download log" ]
                       ]
                 ]
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
        , div [ class "three columns" ]
            [ input [ id "config-notify-on-wave" 
                    , type_ "checkbox"
                    , checked config.notifyOnWave
                    , onCheck ConfigSetNotifyOnWave
                    , disabled (not config.notificationsPermitted)
                    ]
                  []
            , label [ for "config-notify-on-wave" ]
                [ text "Notify me when it's time for a visit" ]
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

date : Config -> Time.Posix -> String
date config time =
    let year  = Time.toYear config.zone time |> String.fromInt
        month = Time.toMonth config.zone time |> toTwoDigitMonth
        day   = Time.toDay config.zone time |> twoDigitInt
    in
    year ++ "-" ++ month ++ "-" ++ day

toTwoDigitMonth : Time.Month -> String
toTwoDigitMonth month =
    case month of
        Time.Jan -> "01"
        Time.Feb -> "02"
        Time.Mar -> "03"
        Time.Apr -> "04"
        Time.May -> "05"
        Time.Jun -> "06"
        Time.Jul -> "07"
        Time.Aug -> "08"
        Time.Sep -> "09"
        Time.Oct -> "10"
        Time.Nov -> "11"
        Time.Dec -> "12"

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

rowCts : String -> List (Html msg) -> Html msg
rowCts cls cts = div [ class "row", class cls ] cts

row : String -> Html msg -> Html msg
row cls cts = rowCts cls [ cts ]

rowLabel : String -> Html msg
rowLabel lbl = h5 [ class "one column"] [ text lbl ]
              
centered : Attribute msg
centered = class "ten columns offset-by-one"

-- fontawesome wrapper

icon : String -> Html msg
icon iClass = node "i" [ class iClass ] []

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
