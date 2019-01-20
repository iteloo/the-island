module BaseType exposing
    ( CardSeed
    , PlayerInfo
    , StageType(..)
    , Uber(..)
    , add
    )

import Material exposing (Material)


type StageType
    = ReadyStageType
    | AuctionStageType
    | TradeStageType


type alias CardSeed =
    Int


type alias PlayerInfo =
    { name : String
    , ready : Bool
    }


type Uber number
    = Finite number
    | Infinite


add : Uber number -> number -> Uber number
add x y =
    case x of
        Finite z ->
            Finite (z + y)

        Infinite ->
            Infinite
