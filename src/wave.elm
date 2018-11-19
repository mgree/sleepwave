{- TODO

   nice rendering
   export to CSV
     https://package.elm-lang.org/packages/elm/file/latest/File-Download
   configure timings, 24-hour clock

   some notion of "end"?
     but what about false starts?
-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Time


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

defaultGracePeriod : Int
defaultGracePeriod =  1000 * 30 -- 30sec grace period

testWaveTime = 1000 * 15
testGracePeriod = 1000 * 5
                      
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

isWaved : (Time.Posix,LogEntry) -> Bool
isWaved (_,entry) =
    case entry of
        Waved _ -> True
        _ -> False
            
countWaves : Model -> Int
countWaves model = List.filter isWaved model.log |> List.length
                
type alias Log = List (Time.Posix, LogEntry)
                
type alias Model = 
  { state : State
  , time : Time.Posix
  , timeBegun : Time.Posix
  , timeEntered : Time.Posix
  , log : Log

    -- configuration
  , zone : Time.Zone
  , waveTime : Int    -- millis
  , graceTime : Int -- millis
  , twelveHour : Bool
  }

log : LogEntry -> Model -> Model
log entry model = { model | log = (model.time,entry)::model.log }

squashCryingStopped : Model -> Model
squashCryingStopped model =
    case model.log of
        (time, CryingStopped) :: oldLog ->
            { model | log = (time, CryingSquashed):: oldLog }
        _ -> model

hasBegun : Model -> Bool
hasBegun model = model.timeBegun /= Time.millisToPosix 0

             
init : () -> (Model, Cmd Msg)
init _ =
  ( { state = Loading
    , time = Time.millisToPosix 0
    , timeBegun = Time.millisToPosix 0
    , timeEntered = Time.millisToPosix 0
    , log = []            
    , zone = Time.utc
    , waveTime = testWaveTime -- defaultWaveTime 
    , graceTime = testGracePeriod -- defaultGracePeriod
    , twelveHour = True
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

checkTimers : Model -> (Model, Cmd Msg)
checkTimers model =
    case model.state of
        -- no interesting timers running
        Loading -> (model, Cmd.none)
        Quiescent -> (model, Cmd.none)
        Waving -> (model, Cmd.none)

        BetweenWaves ->
            if timeDifference model.time model.timeEntered >= model.waveTime
            then -- time for a wave
                ({ model | state = Waving,
                           timeEntered = model.time }
                , Cmd.none)
            else (model, Cmd.none)
                
        GracePeriod originalTime ->  
            if timeDifference model.time model.timeEntered >= model.graceTime
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
                  zone = newZone }
      , Cmd.none
      )

    Begin ->
      ( log Began
            { model | state = Quiescent,
                      timeEntered = model.time,
                      timeBegun = model.time }
      , Cmd.none
      )

    StartWave ->
      ( log CryingStarted
            { model | state = BetweenWaves,
                      timeEntered = model.time }
      , Cmd.none
      )

    EndWave ->
      ( log CryingStopped
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
      ( log (Waved model.timeEntered)
            { model | state = BetweenWaves,
                      timeEntered = model.time }
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
    ]

viewClock : Model -> Html msg
viewClock model = 
    div [ class "clock" ]
        [ h1 [ class "clock" ] [ clockTime model.twelveHour model.zone model.time ] ]
            
viewRemaining : Model -> Html msg
viewRemaining model =
  div [ class "remaining" ]
      (case model.state of
           Loading -> []
           Quiescent -> []
           BetweenWaves ->
               [ div [ class "wave" ]
                     [ text "You should wave at "
                     , remainingTime model model.timeEntered model.waveTime
                     ]
               ]
           Waving ->
               [ div [ class "wave" ]
                     [ text "Time for a wave!" ]
               ]
           GracePeriod originalTimeEntered ->
               [ div [ class "resume" ]
                     [ text "If crying resumes, you should wave at "
                     , remainingTime model originalTimeEntered model.waveTime
                     ]
               , div [ class "grace" ]
                     [ text "Grace period concludes at "
                     , remainingTime model model.timeEntered model.graceTime
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
              , div [ class "waves" ]
                  [ text (countPlural (countWaves model) "wave") ]
              , table [ class "log" ]
                  (List.map (viewLogEntry model) model.log)
              ]
         else [])

viewLogEntry : Model -> (Time.Posix, LogEntry) -> Html msg
viewLogEntry model (time, entry) =
    tr [ class "entry" ]
        [ td [ class "time" ] [ clockTime model.twelveHour model.zone time ]
        , td [ class "event" ]
             ( case entry of
                   Began -> [ text "began" ]
                   CryingStarted -> [ text "crying started" ]
                   CryingStopped -> [ text "crying stopped" ]
                   CryingSquashed -> [ s [] [ text "crying stopped" ] ]
                   Waved timeDue ->
                       [ text "completed wave due at "
                       , clockTime model.twelveHour model.zone timeDue
                       ]
             )
        ]
              
        
        
clockTime : Bool -> Time.Zone -> Time.Posix -> Html msg
clockTime twelveHour zone time = 
  let baseHour  = Time.toHour zone time
      hour      = String.fromInt (if twelveHour then modBy 12 baseHour else baseHour)
      minute    = twoDigitInt    (Time.toMinute zone time)
      second    = twoDigitInt    (Time.toSecond zone time)
      ampm      = if twelveHour
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
        [ clockTime model.twelveHour model.zone targetTime
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
