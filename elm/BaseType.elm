module BaseType exposing
    ( ActionButtonInfo
    , PlayerInfo
    , Site(..)
    , StageType(..)
    , Uber(..)
    , add
    , siteToString
    )

import Material exposing (Material)


type alias PlayerInfo =
    { name : String
    , ready : Bool
    }


type Site
    = Forest
    | Farm
    | Hospital
    | Watchtower
    | Beach


siteToString : Site -> String
siteToString =
    toString


type StageType
    = WaitStageType
    | SiteSelectionStageType
    | SiteVisitStageType
    | GameOverStageType


type alias ActionButtonInfo =
    { actionButtonName : String
    , actionButtonResourceAllocation : String
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
