module Timer
    exposing
        ( Timer
        , State(..)
        , init
        , state
        , timeLeft
        , setTimeLeft
        , update
        , pause
        , resume
        )

import Time exposing (Time)


type Timer
    = Paused_ Data
    | Running_ Data
    | Done_


type State
    = Paused
    | Running
    | Done


type alias Data =
    { lastTick : Maybe Time
    , timeLeft : Time
    }


init : Time -> Timer
init startTime =
    Running_
        { lastTick = Nothing
        , timeLeft = startTime
        }


state : Timer -> State
state timer =
    case timer of
        Paused_ _ ->
            Paused

        Running_ _ ->
            Running

        Done_ ->
            Done


timeLeft : Timer -> Time
timeLeft timer =
    case timer of
        Paused_ { timeLeft } ->
            timeLeft

        Running_ { timeLeft } ->
            timeLeft

        Done_ ->
            0


setTimeLeft : Time -> Timer -> Timer
setTimeLeft timeLeft timer =
    case timer of
        Paused_ rec ->
            Paused_ { rec | timeLeft = timeLeft }

        Running_ rec ->
            Running_ { rec | timeLeft = timeLeft }

        Done_ ->
            Done_


update : Time -> Timer -> Timer
update tick timer =
    case timer of
        Paused_ rec ->
            Paused_ { rec | lastTick = Just tick }

        Running_ rec ->
            if rec.timeLeft < 0 then
                Done_
            else
                Running_
                    { rec
                        | lastTick = Just tick
                        , timeLeft =
                            let
                                lastTick =
                                    Maybe.withDefault tick rec.lastTick
                            in
                                rec.timeLeft - (tick - lastTick)
                    }

        Done_ ->
            timer


pause : Timer -> Timer
pause timer =
    case timer of
        Paused_ _ ->
            timer

        Running_ d ->
            Paused_ d

        Done_ ->
            timer


resume : Timer -> Timer -> Timer
resume tick timer =
    case timer of
        Paused_ d ->
            Running_ d

        Running_ d ->
            timer

        Done_ ->
            timer
