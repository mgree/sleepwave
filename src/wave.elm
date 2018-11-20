{- TODO

   nice rendering
   export to CSV
     https://package.elm-lang.org/packages/elm/file/latest/File-Download

   some notion of "end"?
     but what about false starts?

   change from checkbox to radio boxes

   store settings in localStorage (needs a port)
-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck, onBlur, keyCode, targetValue, on)
import Html.Lazy
import Task
import Time
import Json.Decode as Json

import Parser exposing (Parser, (|.), (|=), succeed, symbol, end, oneOf)


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

defaultWaveTime : Int
defaultWaveTime = 1000 * 60 * 5 -- 5min wave time

defaultGraceTime : Int
defaultGraceTime =  1000 * 30 -- 30sec grace period
                      
type State = Loading
           | Quiescent
           | BetweenWaves
           | Waving
           | GracePeriod Time.Posix -- timeEntered to restore

type LogEntry = Began
              | CryingStarted
              | CryingStopped
              | CryingSquashed
              | Waved Time.Posix -- time was due
              | Debug String

type RemainingMode = ClockAndTimeLeft
                   | ClockOnly
                   | TimeLeftOnly

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

type alias Config =
  { zone : Time.Zone
  , waveTime : Int    -- millis
  , graceTime : Int -- millis
  , twelveHour : Bool
  , remainingMode : RemainingMode

  -- for updates
  , enteredWaveTime : String
  , enteredGraceTime : String
  }

setZone : Time.Zone -> Config -> Config
setZone newZone config = { config | zone = newZone }

setTwelveHour : Bool -> Config -> Config
setTwelveHour newTwelveHour config = { config | twelveHour = newTwelveHour }
                                     
updateWaveTime : String -> Config -> Config
updateWaveTime entered config = { config | enteredWaveTime = entered }
                             
updateGraceTime : String -> Config -> Config
updateGraceTime entered config = { config | enteredGraceTime = entered }

trySetWaveTime : Config -> Config
trySetWaveTime config =
    case tryHMSToMillis config.enteredWaveTime of
        Nothing -> config
        Just new -> { config |
                          waveTime = new,
                          enteredWaveTime = millisToShortTime new }

trySetGraceTime : Config -> Config
trySetGraceTime config =
    case tryHMSToMillis config.enteredGraceTime of
        Nothing -> config
        Just new -> { config |
                          graceTime = new,
                          enteredGraceTime = millisToShortTime new }

cycleRemainingMode : Config -> Config
cycleRemainingMode config =
    { config | remainingMode = nextRemainingMode config.remainingMode }
                    
type alias Model = 
  { state : State
  , time : Time.Posix
  , timeBegun : Time.Posix
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

initConfig : Config
initConfig =
    { zone = Time.utc
    , waveTime = defaultWaveTime 
    , graceTime = defaultGraceTime
    , twelveHour = True
    , remainingMode = ClockAndTimeLeft
    , enteredWaveTime = millisToShortTime defaultWaveTime
    , enteredGraceTime = millisToShortTime defaultGraceTime
    }
                 
init : () -> (Model, Cmd Msg)
init _ =
  ( { state = Loading
    , time = Time.millisToPosix 0
    , timeBegun = Time.millisToPosix 0
    , timeEntered = Time.millisToPosix 0
    , log = []
    , config = initConfig
    }
  , Task.perform (\x -> x) (Task.map2 InitializeTime Time.here Time.now)
  )


-- UPDATE


type Msg
  = Tick Time.Posix
  | InitializeTime Time.Zone Time.Posix
  | Begin
  | StartWave
  | EndWave
  | ResumeWave Time.Posix
  | Wave
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
        
    ConfigSetTwelveHour newTwelveHour ->
      ( { model | config = setTwelveHour newTwelveHour model.config }
      , Cmd.none)
        
    ConfigUpdateWaveTime newWaveTime ->
      ( { model | config = updateWaveTime newWaveTime model.config }
      , Cmd.none)
        
    ConfigUpdateGraceTime newGraceTime ->
      ( { model | config = updateGraceTime newGraceTime model.config }
      , Cmd.none)
        
    ConfigSetWaveTime ->
      ( { model | config = trySetWaveTime model.config }
      , Cmd.none)

    ConfigSetGraceTime ->
      ( { model | config = trySetGraceTime model.config }
      , Cmd.none)

    ConfigCycleRemaining ->
      ( { model | config = cycleRemainingMode model.config }
      , Cmd.none)

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
    , Html.Lazy.lazy2 (\cfg lg -> viewLog cfg lg |> row "log")
        model.config model.log
    , Html.Lazy.lazy (viewConfig >> row "config") model.config
    ]

viewClock : Model -> Html msg
viewClock model =
    h1 [ class "clock"
       , centered ]
       [ clockTime model.config model.time ]
            
viewRemaining : Model -> Html Msg
viewRemaining model =
    let computed = computeTimeLeft model model.timeEntered model.config.waveTime in
    div [ class "ten columns offset-by-one" ]
        (case model.state of
             Loading -> []
             Quiescent -> []
             BetweenWaves ->
                 [ div [ class "next-wave" ]
                       [ remainingTime model.config computed ]
                 ]
             Waving ->
                 [ h1 [ class "wave" ]
                      [ text "time for a visit" ]
                 ]
             GracePeriod originalTimeEntered ->
               [ div [ class "resume" ]
                     [ remainingTime model.config computed
                     ]
               , div [ class "grace" ]
                     [ timeLeft (computeTimeLeft model
                                     model.timeEntered model.config.graceTime)
                           "left in grace period"
                     ]
               ]
      )

remainingTime : Config -> (Int, Time.Posix) -> Html Msg
remainingTime config computed =
    let clock = targetTimeClock config computed
        lbl   = span [] [ text "next wave" ]
        left  = timeLeft computed ""
    in
        div [ class "remaining"
            , onClick ConfigCycleRemaining
            ]
            (case config.remainingMode of
                 ClockAndTimeLeft -> [ lbl, clock, left ]
                 ClockOnly        -> [ lbl, clock ]
                 TimeLeftOnly     -> [ lbl,        left ])

timeLeft : (Int, Time.Posix) -> String -> Html msg
timeLeft (remaining, targetTime) msg =
    h4 [class "timeleft"]
        [ text ("" ++ millisToLongTime remaining ++ " " ++ msg)
        ]
        
targetTimeClock : Config -> (Int, Time.Posix) -> Html msg
targetTimeClock config (remaining, targetTime) =
    h1 [class "target"] [ clockTime config targetTime ]
        
computeTimeLeft : Model -> Time.Posix -> Int -> (Int, Time.Posix)
computeTimeLeft model timeStarted duration =
    let elapsed    = timeDifference model.time timeStarted
        remaining  = duration - elapsed
        targetTime = Time.posixToMillis model.time + remaining |> Time.millisToPosix
    in
        (remaining, targetTime)
        
viewActions : Model -> Html Msg
viewActions model =
    let actionClass handler = [ centered, handler ] in
    case model.state of
        Loading ->
            button (actionClass (onClick Begin)) [ text "begin" ]
             
        Quiescent ->
            button (actionClass (onClick StartWave)) [ text "crying started" ]
        
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
                    [ text "You've been doing the sleep wave for "
                    , text (millisToLongTime
                                (timeDifference model.time model.timeBegun))
                    , text "."
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

viewLog : Config -> Log -> Html msg
viewLog config log =
    table [ class "log ten columns offset-by-one" ]
        (if List.isEmpty log
         then []
         else
             [ thead []
                   [ tr []
                         [ td [] [ text "Time" ]
                         , td [] [ text "Event" ] ]
                   ]
             , tbody []
                 (List.map (viewLogEntry config) log)
             ])
            
viewLogEntry : Config -> (Time.Posix, LogEntry) -> Html msg
viewLogEntry config (time, entry) =
    tr [ class "entry" ]
        [ td [ class "time" ] [ clockTime config time ]
        , td [ class "event" ]
             ( case entry of
                   Began -> [ text "began" ]
                   CryingStarted -> [ text "crying started" ]
                   CryingStopped -> [ text "crying stopped" ]
                   CryingSquashed -> [ s [] [ text "crying stopped" ] ]
                   Waved timeDue ->
                       [ text "completed wave due at "
                       , clockTime config timeDue
                       ]
                   Debug s -> [ text s ]
             )
        ]

viewConfig : Config -> Html Msg
viewConfig config =
    div [ ]
        [ rowLabel "Settings"
        , div [ class "two columns offset-by-two" ]
            (timeInput config .enteredWaveTime .waveTime
                 "config-wave-duration" "Wave duration"
                 ConfigUpdateWaveTime ConfigSetWaveTime)
        , div [ class "two columns" ]
            (timeInput config .enteredGraceTime .graceTime
                 "config-grace-time" "Grace period"
                 ConfigUpdateGraceTime ConfigSetGraceTime)
        , div [ class "two columns" ]
            [ label [ for "config-twelve-hour" ]
                  [ text "Use 12-hour clock (am/pm)" ]
            , input [ id "config-twelve-hour"
                    , type_ "checkbox"
                    , checked config.twelveHour
                    , onCheck ConfigSetTwelveHour
                    ]
                  []
            ]
        ]

timeInput : Config -> (Config -> String) -> (Config -> Int) -> String -> String -> (String -> Msg) -> Msg -> List (Html Msg)
timeInput config entered actual inputId labelText updateMsg setMsg =
    let inSync = (entered config) == millisToShortTime (actual config) in
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
  let isEnter code = if code == 13 then Json.succeed "" else Json.fail ""
      decodeEnter = Json.andThen isEnter keyCode
  in
      on "keypress" (Json.map (\key -> msg) decodeEnter)
        
clockTime : Config -> Time.Posix -> Html msg
clockTime config time = 
  let baseHour  = Time.toHour config.zone time
      hour      = String.fromInt (if config.twelveHour
                                  then modBy 12 baseHour
                                  else baseHour)
      minute    = twoDigitInt    (Time.toMinute config.zone time)
      second    = twoDigitInt    (Time.toSecond config.zone time)
      ampm      = if config.twelveHour
                  then if baseHour > 12
                       then "pm"
                       else "am"
                  else ""
  in
  text (hour ++ ":" ++ minute ++ ":" ++ second ++ ampm)
                        
millisToLongTime : Int -> String
millisToLongTime millis =
    let (hours, minutes, seconds) = millisToHMS millis in
    let strs = [ countPlural hours "hour"
               , countPlural minutes "minute"
               , countPlural seconds "second"
               ]
    in
    List.filter (not << String.isEmpty) strs |> String.join " "
        
millisToShortTime : Int -> String
millisToShortTime millis =
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
