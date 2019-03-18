module Update exposing (subscriptions, update)

import AnimationFrame
import Api
import BaseType exposing (..)
import Debug
import Helper
import Lens exposing (Lens, goIn)
import Material exposing (Material)
import Model exposing (..)
import Msg exposing (..)
import Random
import Server
import Shake
import Time exposing (Time)
import Timer


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        case model.app of
            JoinGameScreen m ->
                case m.submittedName of
                    Just gameName ->
                        [ Server.listen model
                            gameName
                            (AppMsg << ServerMsgReceived)
                        ]

                    Nothing ->
                        []

            GameScreen m ->
                [ Server.listen model
                    m.gameName
                    (AppMsg << ServerMsgReceived)
                , Shake.shake
                    (AppMsg
                        << GameMsg
                        << always Shake
                    )
                , case m.timer of
                    Just _ ->
                        AnimationFrame.times (AppMsg << GameMsg << UpdateTimer)

                    Nothing ->
                        Sub.none
                ]


type alias Eff a =
    ( a, Cmd Msg )


type alias Upd model =
    model -> Eff model


type alias Ctx msg =
    { toServer : String -> Server.SendToServer
    , toMsg : msg -> Msg
    }


type alias GameCtx msg =
    { toGameServer : Server.SendToServer
    , toMsg : msg -> Msg
    }


update : Msg -> Upd Model
update msg model =
    case msg of
        AppMsg msg ->
            let
                ( m, cmd ) =
                    updateApp
                        { toServer = Server.send model
                        , toMsg = AppMsg
                        }
                        msg
                        model.app
            in
            ( { model | app = m }, cmd )


updateApp : Ctx AppMsg -> AppMsg -> Upd AppModel
updateApp ctx msg model =
    case msg of
        JoinGameMsg msg ->
            tryUpdate joinGame
                (updateWelcome { ctx | toMsg = ctx.toMsg << JoinGameMsg } msg)
                model

        GameMsg msg ->
            tryUpdate game
                (updateGame { ctx | toMsg = ctx.toMsg << GameMsg } msg)
                model

        ServerMsgReceived action ->
            case action of
                Ok action ->
                    model
                        |> handleAction action

                Err e ->
                    Debug.crash <| "Error in ServerMsgReceived: " ++ e


updateWelcome : Ctx JoinGameMsg -> JoinGameMsg -> Upd JoinGameModel
updateWelcome { toServer } msg model =
    case msg of
        JoinGameButton ->
            let
                gameName =
                    model.gameNameInput
            in
            { model | submittedName = Just gameName }
                ! [ {- [question] sending Api.JoinGame even necessary?
                       or does the server add us to the game automatically
                       upon ws connection?
                    -}
                    toServer gameName (Api.JoinGame gameName)
                  ]

        GameNameInputChange str ->
            { model | gameNameInput = str } ! []


updateGame : Ctx GameMsg -> GameMsg -> Upd GameModel
updateGame { toServer, toMsg } msg model =
    let
        toGameServer =
            toServer model.gameName
    in
    case msg of
        WaitMsg msg ->
            case msg of
                Ready _ ->
                    model
                        ! [ toGameServer (Api.Ready True) ]

                NameInputChange name ->
                    { model | playerName = name }
                        ! [ toGameServer (Api.SetName name) ]

        SiteSelectionMsg msg ->
            tryUpdate siteSelection
                (updateSiteSelection
                    { toGameServer = toGameServer
                    , toMsg = toMsg << SiteSelectionMsg
                    }
                    msg
                )
                model

        SiteVisitMsg msg ->
            tryUpdate siteVisit
                (updateSiteVisit
                    { toGameServer = toGameServer
                    , toMsg = toMsg << SiteVisitMsg
                    }
                    msg
                )
                model

        UpdateTimer tick ->
            { model
                | timer = Maybe.map (Timer.update tick) model.timer
            }
                ! []

        MoveToBasket resource count ->
            case Helper.move resource count model.inventory model.basket of
                Nothing ->
                    Debug.crash
                        "+/- buttons should be disabled"

                Just ( newInv, newBasket ) ->
                    { model
                        | inventory = newInv
                        , basket = newBasket
                    }
                        ! []

        EmptyBasket ->
            { model
                | inventory =
                    Material.map2 (always (+))
                        model.basket
                        model.inventory
                , basket = Material.empty
            }
                ! []

        Shake ->
            model ! [ toGameServer (Api.Trade model.basket) ]


updateSiteSelection :
    GameCtx SiteSelectionMsg
    -> SiteSelectionMsg
    -> Upd SiteSelectionModel
updateSiteSelection { toGameServer } msg model =
    case msg of
        SiteSelected site ->
            { model | siteSelected = Just site }
                ! [ toGameServer <| Api.SiteSelected site ]

        ReadyButton ->
            { model | ready = not model.ready } ! []


updateSiteVisit :
    GameCtx SiteVisitMsg
    -> SiteVisitMsg
    -> Upd SiteVisitModel
updateSiteVisit { toGameServer } msg model =
    let
        ifEvent handler =
            case model.event of
                Just e ->
                    handler e

                Nothing ->
                    Debug.crash "no event, but buttons pressed"

        tryModifyResourceAmountSelected diff =
            ifEvent <|
                \e ->
                    -- [tofix] impl
                    { model
                        | event =
                            Just
                                { e
                                    | resourceAmountSelected =
                                        e.resourceAmountSelected + diff
                                }
                    }
                        ! []
    in
    case msg of
        OkButton ->
            ifEvent <|
                \e ->
                    model
                        ! [ toGameServer <|
                                Api.EventResponse
                                    { messageId = e.messageId
                                    , clickedOk = True
                                    , clickedAction = False
                                    , resourceAmount =
                                        e.resourceAmountSelected
                                    }
                          ]

        AddResourceSpendButton ->
            tryModifyResourceAmountSelected 1

        RemoveResourceSpendButton ->
            tryModifyResourceAmountSelected -1

        ActionButton ->
            ifEvent <|
                \e ->
                    model
                        ! [ toGameServer <|
                                Api.EventResponse
                                    { messageId = e.messageId
                                    , clickedOk = False
                                    , clickedAction = True
                                    , resourceAmount =
                                        case e.actionButton of
                                            Nothing ->
                                                0

                                            Just ab ->
                                                ab.actionButtonResourceAmount
                                    }
                          ]


handleAction : Api.Action -> Upd AppModel
handleAction action model =
    case action of
        Api.Welcome name ->
            GameScreen (initGameModel name) ! []

        Api.GameStateChanged stage ->
            tryUpdate game (changeStage stage) model

        Api.SetClock ms ->
            tryUpdate game
                (\m ->
                    { m
                        | timer =
                            Just <|
                                let
                                    time =
                                        toFloat ms * Time.millisecond
                                in
                                case m.timer of
                                    Nothing ->
                                        Timer.init time

                                    Just timer ->
                                        Timer.setTimeLeft time timer
                    }
                        ! []
                )
                model

        Api.PlayerInfoUpdated info ->
            tryUpdate (game |> goIn wait)
                (\m -> { m | playerInfo = info } ! [])
                model

        Api.TradeCompleted mat ->
            tryUpdate game
                (\m -> { m | basket = mat } ! [])
                model

        Api.Event e ->
            tryUpdate (game |> goIn siteVisit)
                (\m ->
                    { m
                        | event =
                            Just
                                { messageId = e.messageId
                                , title = e.title
                                , description = e.description
                                , okButton = e.okButton
                                , spendButton = e.spendButton
                                , actionButton = e.actionButton
                                , resourceAmountSelected = 0
                                }
                    }
                        ! []
                )
                model

        Api.GameOver winner ->
            model ! []


changeStage : StageType -> Upd GameModel
changeStage stagetype model =
    let
        oldStage =
            model.stage

        ( newStage, cmd ) =
            case ( oldStage, stagetype ) of
                ( _, SiteSelectionStageType ) ->
                    SiteSelectionStage initSiteSelectionModel ! []

                ( SiteSelectionStage old, SiteVisitStageType ) ->
                    case old.siteSelected of
                        Just site ->
                            SiteVisitStage (initSiteVisitModel site) ! []

                        Nothing ->
                            Debug.crash "No site selected before transition"

                _ ->
                    Debug.crash
                        ("Invalid stage transtion from "
                            ++ toString (getStageType oldStage)
                            ++ " to "
                            ++ toString stagetype
                        )
    in
    ( { model
        | stage = newStage
        , timer = Nothing
      }
    , cmd
    )



-- HELPER UPDATERS


type alias EffLens submodel model =
    Lens submodel (Eff submodel) model (Eff model)


joinGame : EffLens JoinGameModel AppModel
joinGame =
    let
        get model =
            case model of
                JoinGameScreen m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( JoinGameScreen m, cmd )
    in
    { get = get, set = set }


game : EffLens GameModel AppModel
game =
    let
        get model =
            case model of
                GameScreen m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( GameScreen m, cmd )
    in
    { get = get, set = set }


wait : EffLens WaitModel GameModel
wait =
    let
        get model =
            case model.stage of
                WaitStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( { model | stage = WaitStage m }, cmd )
    in
    { get = get, set = set }


siteSelection : EffLens SiteSelectionModel GameModel
siteSelection =
    let
        get model =
            case model.stage of
                SiteSelectionStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( { model | stage = SiteSelectionStage m }, cmd )
    in
    { get = get, set = set }


siteVisit : EffLens SiteVisitModel GameModel
siteVisit =
    let
        get model =
            case model.stage of
                SiteVisitStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( { model | stage = SiteVisitStage m }, cmd )
    in
    { get = get, set = set }


tryUpdate :
    Lens submodel updatedSubmodel model (Eff model)
    -> (submodel -> updatedSubmodel)
    -> model
    -> Eff model
tryUpdate lens upd model =
    case Lens.update lens upd model of
        Just m ->
            m

        Nothing ->
            Debug.crash "tryUpdate failed"


updateIf :
    Lens submodel updatedSubmodel model (Eff model)
    -> (submodel -> model -> Eff model)
    -> model
    -> Eff model
updateIf lens upd model =
    case Lens.updateIf lens upd model of
        Just m ->
            m

        Nothing ->
            Debug.crash "tryUpdate failed"
