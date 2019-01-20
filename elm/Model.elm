module Model exposing
    ( AppModel(..)
    , GameModel
    , JoinGameModel
    , Model
    , SiteSelectionModel
    , SiteVisitModel
    , Stage(..)
    , WaitModel
    , getStageType
    , initAppModel
    , initGameModel
    , initJoinGameModel
    , initModel
    , initSiteSelectionModel
    , initSiteVisitModel
    , initWaitModel
    , timer
    )

import Api
import BaseType exposing (..)
import Lens exposing (PureLens)
import Material exposing (Material, Resource)
import Time exposing (Time)
import Timer exposing (Timer)


siteSelectionDuration =
    10 * Time.second


type alias Model =
    { hostname : String
    , app : AppModel
    }


type AppModel
    = JoinGameScreen JoinGameModel
    | GameScreen GameModel


type alias JoinGameModel =
    { gameNameInput : String

    {- [tmp] [hack] right now this is needed
       because we can only listen to ws
       once we know the name, otherwise
       the server will add us to a random
       game

       [note] we aren't using a bool, in case
       input changes while waiting for server
       response (though unlikely)
    -}
    , submittedName : Maybe String
    }


type alias GameModel =
    { gameName : String
    , playerName : String
    , stage : Stage
    , inventory : Material Int
    , basket : Material Int
    }


type Stage
    = -- [todo] move into AppModel to avoid storing bogus game info
      -- currently kept like this to utilise existing code
      WaitStage WaitModel
    | SiteSelectionStage SiteSelectionModel
    | SiteVisitStage SiteVisitModel
    | GameOverStage


type alias WaitModel =
    { ready : Bool
    , playerInfo : List PlayerInfo
    }


type alias SiteSelectionModel =
    { ready : Bool
    , siteSelected : Maybe Site
    , timer : Timer
    }


type alias SiteVisitModel =
    { site : Site
    , event : Maybe Event
    }


type alias Event =
    Extra Api.EventMessage


{-| [hack] is there a cleaner way to extend records?
-}
type alias Extra a =
    { a
        | timer : Timer
        , resourceAmount : Int
    }


initModel : String -> Model
initModel hostname =
    { hostname = hostname
    , app = initAppModel
    }


initAppModel : AppModel
initAppModel =
    JoinGameScreen initJoinGameModel


initJoinGameModel : JoinGameModel
initJoinGameModel =
    { gameNameInput = ""
    , submittedName = Nothing
    }


initGameModel : String -> GameModel
initGameModel name =
    { gameName = name
    , playerName = "Anonymous"
    , stage = WaitStage initWaitModel
    , inventory = Material.empty
    , basket = Material.empty
    }


initWaitModel : WaitModel
initWaitModel =
    { ready = False
    , playerInfo = []
    }


initSiteSelectionModel =
    { ready = False
    , siteSelected = Nothing
    , timer = Timer.init siteSelectionDuration
    }


initSiteVisitModel site =
    { site = site
    , event = Nothing
    }


getStageType : Stage -> StageType
getStageType stage =
    case stage of
        WaitStage _ ->
            WaitStageType

        SiteSelectionStage _ ->
            SiteVisitStageType

        SiteVisitStage _ ->
            SiteVisitStageType

        GameOverStage ->
            GameOverStageType



-- GETTER & SETTERS


timer : PureLens Timer Stage
timer =
    let
        get stage =
            case stage of
                WaitStage _ ->
                    Nothing

                SiteSelectionStage m ->
                    Just m.timer

                SiteVisitStage m ->
                    m.event |> Maybe.map .timer

                GameOverStage ->
                    Nothing

        set timer stage =
            case stage of
                WaitStage _ ->
                    Nothing

                SiteSelectionStage m ->
                    Just <| SiteSelectionStage { m | timer = timer }

                SiteVisitStage m ->
                    Just <|
                        SiteVisitStage
                            { m
                                | event =
                                    Maybe.map
                                        (\e -> { e | timer = timer })
                                        m.event
                            }

                GameOverStage ->
                    Nothing
    in
    { get = get, set = set }
