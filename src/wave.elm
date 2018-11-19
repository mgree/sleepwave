{- TODO

   nice rendering
   export to CSV
     https://package.elm-lang.org/packages/elm/file/latest/File-Download

   some notion of "end"?
     but what about false starts?
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
  }

setZone : Time.Zone -> Config -> Config
setZone newZone config = { config | zone = newZone }

setTwelveHour : Bool -> Config -> Config
setTwelveHour newTwelveHour config = { config | twelveHour = newTwelveHour }

trySetTime : String -> (Int -> Config -> Config) -> Config -> Config
trySetTime str setter config =
    case tryHMSToMillis str of
        Nothing -> config
        Just newValue -> setter newValue config
                                     
setWaveTime : Int -> Config -> Config
setWaveTime newWaveTime config = { config | waveTime = newWaveTime }

setGraceTime : Int -> Config -> Config
setGraceTime newGraceTime config = { config | graceTime = newGraceTime }

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
  | ConfigSetWaveTime String
  | ConfigSetGraceTime String
  | ConfigSetTwelveHour Bool

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
      , Cmd.none
      )

    Begin ->
      ( logEntry Began
            { model | state = Quiescent,
                      timeEntered = model.time,
                      timeBegun = model.time }
      , Cmd.none
      )

    StartWave ->
      ( logEntry CryingStarted
            { model | state = BetweenWaves,
                      timeEntered = model.time }
      , Cmd.none
      )

    EndWave ->
      ( logEntry CryingStopped
            { model | state = GracePeriod model.timeEntered,
                      timeEntered = model.time }
      , Cmd.none
      )

    ResumeWave originalTimeEntered ->
      ( squashCryingStopped
            { model | state = BetweenWaves,
                      timeEntered = originalTimeEntered }
      , Cmd.none
      )

    Wave ->
      ( logEntry (Waved model.timeEntered)
            { model | state = BetweenWaves,
                      timeEntered = model.time }
      , Cmd.none)

    ConfigSetTwelveHour newTwelveHour ->
      ( { model | config = setTwelveHour newTwelveHour model.config }
      , Cmd.none)

    ConfigSetWaveTime newWaveTime ->
      ( { model | config = trySetTime newWaveTime setWaveTime model.config }
      , Cmd.none)
        
    ConfigSetGraceTime newGraceTime ->
      ( { model | config = trySetTime newGraceTime setGraceTime model.config }
      , Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 250 Tick


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewClock model
    , viewRemaining model
    , viewActions model
    , viewInfo model
    , Html.Lazy.lazy viewConfig model.config
    ]

viewClock : Model -> Html msg
viewClock model = 
    div [ class "clock" ]
        [ h1 [ class "clock" ] [ clockTime model.config model.time ] ]
            
viewRemaining : Model -> Html msg
viewRemaining model =
  div [ class "remaining" ]
      (case model.state of
           Loading -> []
           Quiescent -> []
           BetweenWaves ->
               [ div [ class "wave" ]
                     [ text "You should wave at "
                     , remainingTime model model.timeEntered model.config.waveTime
                     ]
               ]
           Waving ->
               [ div [ class "wave" ]
                     [ text "Time for a wave!" ]
               ]
           GracePeriod originalTimeEntered ->
               [ div [ class "resume" ]
                     [ text "If crying resumes, you should wave at "
                     , remainingTime model originalTimeEntered model.config.waveTime
                     ]
               , div [ class "grace" ]
                     [ text "Grace period concludes at "
                     , remainingTime model model.timeEntered model.config.graceTime
                     ]
               ]
      )

viewActions : Model -> Html Msg
viewActions model =
    div [ class "action" ]
        (case model.state of
              Loading ->
                  [ button [ onClick Begin ]
                        [ text "Begin" ] ]
                   
              Quiescent ->
                  [ button [ onClick StartWave ]
                        [ text "Crying started" ] ]
              
              BetweenWaves ->
                  [ button [ onClick EndWave ]
                        [ text "Crying stopped" ] ]
             
              Waving ->
                  [ button [ onClick Wave ]
                      [ text "I waved" ]
                  ]

              GracePeriod originalTimeEntered ->
                  [ button [ onClick (ResumeWave originalTimeEntered) ]
                        [ text "Crying restarted" ]
                  ]
        )

viewInfo : Model -> Html msg
viewInfo model =
    div [ class "info" ]
        (if hasBegun model
         then [ div [ class "duration" ]
                    [ text "You've been doing the sleep wave for "
                    , text (millisToLongTime
                                (timeDifference model.time model.timeBegun))
                    ]
              , Html.Lazy.lazy viewWaveCount (countWaves model)
              , Html.Lazy.lazy2 viewLog model.config model.log
              ]
         else [])

viewWaveCount : Int -> Html msg
viewWaveCount numWaves =
    div [ class "waves" ]
        [ text (countPlural numWaves "wave") ]

viewLog : Config -> Log -> Html msg
viewLog config log =
    table [ class "log" ]
        (List.map (viewLogEntry config) log)
            
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
    div [ class "config" ]
        [ timeField config .waveTime "Wave duration" ConfigSetWaveTime
        , timeField config .graceTime "Grace period" ConfigSetGraceTime
        , label [] [ text "Use 12-hour clock (am/pm)"
                   , input [ type_ "checkbox"
                           , checked config.twelveHour
                           , onCheck ConfigSetTwelveHour
                           ]
                         []
                   ]
        ]

timeField : Config -> (Config -> Int) -> String -> (String -> Msg) -> Html Msg
timeField config getter labelText msg =
    label [] [ text labelText
             , input
                   [ type_ "text"
                   , value (millisToShortTime (getter config))
                   , onEnterValue msg
                   , onBlurValue msg
                   ]
                   []
             ]

onBlurValue : (String -> msg) -> Attribute msg
onBlurValue msg =
  on "blur" (Json.map msg targetValue)
        
onEnterValue : (String -> msg) -> Attribute msg
onEnterValue msg =
  let isEnter code = if code == 13 then Json.succeed "" else Json.fail ""
      decodeEnter = Json.andThen isEnter keyCode
  in
      on "keypress" (Json.map2 (\key value -> msg value) decodeEnter targetValue)
        
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

      
remainingTime : Model -> Time.Posix -> Int -> Html msg
remainingTime model timeStarted duration =
    let elapsed    = timeDifference model.time timeStarted
        remaining  = duration - elapsed
        targetTime = Time.posixToMillis model.time + remaining
                                   |> Time.millisToPosix
    in
    span []
        [ clockTime model.config targetTime
        , text (" (" ++ millisToLongTime remaining ++ " remaining)")
        ]
                        
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

-- H:M:S parser             

tryHMSToMillis : String -> Maybe Int
tryHMSToMillis str =
    case Parser.run parseHMS str of
        Err _ -> Nothing
        Ok (h,m,s) -> Just (1000 * (s + 60 * (m + 60 * h)))

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
