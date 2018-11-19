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

twoDigitInt : Int -> String
twoDigitInt n = String.padLeft 2 '0' (String.fromInt n)

countPlural : Int -> String -> String
countPlural n thing =
    case n of
        0 -> ""
        1 -> "1 " ++ thing
        _ -> String.fromInt n ++ " " ++ thing ++ "s"

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
           | GracePeriod Time.Posix

type alias Model = 
  { state : State
  , time : Time.Posix
  , timeEntered : Time.Posix

    -- configuration
  , zone : Time.Zone
  , waveTime : Int    -- millis
  , graceTime : Int -- millis
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Loading (Time.millisToPosix 0) (Time.millisToPosix 0)
          Time.utc testWaveTime testGracePeriod -- defaultWaveTime defaultGracePeriod
  , Task.perform (\x -> x) (Task.map2 InitializeTime Time.here Time.now)
  )


-- UPDATE


type Msg
  = Tick Time.Posix
  | InitializeTime Time.Zone Time.Posix
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
                ({ model | state = Waving }
                , Cmd.none)
            else (model, Cmd.none)
                
        GracePeriod originalTime ->  
            if timeDifference model.time model.timeEntered >= model.graceTime
            then -- great, no need for a wave
                ({ model | state = Quiescent }
                , Cmd.none)
            else (model, Cmd.none)
            
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime -> checkTimers { model | time = newTime }

    InitializeTime newZone newTime  ->
      ( { model |
              state = Quiescent,
              time = newTime,
              timeEntered = newTime,
              zone = newZone }
      , Cmd.none
      )

    StartWave ->
      ( { model |
              state = BetweenWaves,
              timeEntered = model.time }
      , Cmd.none
      )

    EndWave ->
      ( { model |
              state = GracePeriod model.timeEntered,
              timeEntered = model.time }
      , Cmd.none
      )

    ResumeWave originalTimeEntered ->
      ( { model |
              state = BetweenWaves,
              timeEntered = originalTimeEntered }
      , Cmd.none
      )

    Wave ->
      ( { model |
              state = BetweenWaves,
              timeEntered = model.time }
      , Cmd.none)
        

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewClock model.zone model.time
    , viewRemaining model
    , viewActions model
    ]

viewClock : Time.Zone -> Time.Posix -> Html msg
viewClock zone time = 
    div [ class "clock" ]
        [ h1 [ class "clock" ] [ text (clockTime zone time) ] ]
            
clockTime : Time.Zone -> Time.Posix -> String
clockTime zone time = 
  let hour   = String.fromInt (Time.toHour   zone time)
      minute = twoDigitInt    (Time.toMinute zone time)
      second = twoDigitInt    (Time.toSecond zone time)
  in
  hour ++ ":" ++ minute ++ ":" ++ second

viewRemaining : Model -> Html msg
viewRemaining model =
  div [ class "remaining" ]
      (case model.state of
           Loading -> []
           Quiescent -> []
           BetweenWaves ->
               [ div [ class "wave" ]
                     [ text "You should wave at "
                     , text (remainingTime model.zone
                                model.time model.timeEntered model.waveTime)]
               ]
           Waving ->
               [ div [ class "wave" ]
                     [ text "Time for a wave!" ]
               ]
           GracePeriod originalTimeEntered ->
               [ div [ class "resume" ]
                     [ text "If crying resumes, you should wave at "
                     , text (remainingTime model.zone
                                 model.time originalTimeEntered model.waveTime)
                     ]
               , div [ class "grace" ]
                     [ text "Grace period concludes at "
                     , text (remainingTime model.zone
                                 model.time model.timeEntered model.graceTime)
                     ]
               ]
      )
      
remainingTime : Time.Zone -> Time.Posix -> Time.Posix -> Int -> String
remainingTime zone timeNow timeStarted duration =
    let elapsed = timeDifference timeNow timeStarted in
    let remaining = duration - elapsed in
    let targetTime = Time.millisToPosix (Time.posixToMillis timeNow + remaining) in
    clockTime zone targetTime ++
        " (" ++ millisToLongTime remaining ++ " remaining)"
                
viewActions : Model -> Html Msg
viewActions model =
    div [ class "action" ]
        (case model.state of
              Loading -> [ text "Loading..." ]
                   
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
                        [ text "Restart wave" ]
                  ]
        )
        
millisToLongTime : Int -> String
millisToLongTime millis =
    let (hours, minutes, seconds) = millisToHMS millis in
    let strs = [ countPlural hours "hour"
               , countPlural minutes "minute"
               , countPlural seconds "second"
               ] in
    List.filter (not << String.isEmpty) strs |> String.join " "
        
millisToShortTime : Int -> String
millisToShortTime millis =
    let (hours, minutes, seconds) = millisToHMS millis in
    (if hours > 0 then String.fromInt hours ++ ":" else "") ++
    (if minutes > 0
     then (if hours > 0 then twoDigitInt minutes else String.fromInt minutes) ++ ":"
     else "") ++
    (if minutes > 0 then twoDigitInt seconds else String.fromInt seconds)

millisToHMS : Int -> (Int, Int, Int)
millisToHMS millis =              
    let totalSeconds = millis // 1000 in
    let seconds = modBy 60 totalSeconds in
    let totalMinutes = totalSeconds // 60 in
    let minutes = modBy 60 totalMinutes in
    let hours = totalMinutes // 60 in
    (hours, minutes, seconds)
        
viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
         
