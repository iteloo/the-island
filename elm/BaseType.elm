module BaseType exposing
    ( ActionButtonInfo
    , PlayerInfo
    , Site(..)
    , StageType(..)
    , Uber(..)
    , actionButtonMaterial
    , add
    , allSites
    , siteToString
    )

import Material exposing (Material, Resource)
import String.Case


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


allSites =
    [ Forest, Farm, Hospital, Watchtower, Beach ]


siteToString : Site -> String
siteToString =
    String.Case.toSnakeCaseLower << toString


type StageType
    = WaitStageType
    | SiteSelectionStageType
    | SiteVisitStageType
    | GameOverStageType


type alias ActionButtonInfo =
    { actionButtonText : String
    , actionButtonResource : Resource
    , actionButtonResourceAmount : Int
    }


actionButtonMaterial : ActionButtonInfo -> Material Int
actionButtonMaterial { actionButtonResource, actionButtonResourceAmount } =
    Material.empty
        |> Material.set actionButtonResource actionButtonResourceAmount


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
