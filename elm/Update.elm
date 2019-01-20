module Update exposing (subscriptions, update)

import AnimationFrame
import Api
import BaseType exposing (..)
import Card exposing (Card)
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
import ZoomList


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        case model.app of
            WelcomeScreen m ->
                case m.submittedName of
                    Just gameName ->
                        [ Server.listen model
                            gameName
                            (AppMsg << ServerMsgReceived)
                        ]

                    Nothing ->
                        []

            Game m ->
                [ Server.listen model
                    m.gameName
                    (AppMsg << ServerMsgReceived)
                , case Lens.get timer m.stage of
                    Just _ ->
                        AnimationFrame.times (AppMsg << GameMsg << UpdateTimer)

                    Nothing ->
                        Sub.none
                , case m.stage of
                    TradeStage _ ->
                        Sub.batch
                            [ Shake.shake
                                (AppMsg
                                    << GameMsg
                                    << TradeMsg
                                    << always Shake
                                )
                            , Time.every Time.second
                                (AppMsg
                                    << GameMsg
                                    << TradeMsg
                                    << always Yield
                                )
                            ]

                    _ ->
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
        WelcomeMsg msg ->
            tryUpdate welcome
                (updateWelcome { ctx | toMsg = ctx.toMsg << WelcomeMsg } msg)
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
                    model ! []


updateWelcome : Ctx WelcomeMsg -> WelcomeMsg -> Upd WelcomeModel
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
        ReadyMsg msg ->
            case msg of
                Ready _ ->
                    model
                        ! [ toGameServer (Api.Ready True) ]

                NameInputChange name ->
                    { model | name = name }
                        ! [ toGameServer (Api.SetName name) ]

        AuctionMsg msg ->
            tryUpdate auction
                (updateAuction
                    { toGameServer = toGameServer
                    , toMsg = toMsg << AuctionMsg
                    }
                    msg
                )
                model

        TradeMsg msg ->
            handleTradeMsg
                { toGameServer = toGameServer
                , toMsg = AppMsg << GameMsg << TradeMsg
                }
                msg
                model

        UpdateCards cards ->
            { model | cards = cards } ! []

        ActivateButton ->
            case Helper.tryApplyZoomCardEffect toGameServer model of
                Ok r ->
                    r

                Err e ->
                    Debug.crash ("Card activation error: " ++ e)

        UpdateTimer tick ->
            { model
                | stage =
                    Lens.update timer
                        (Timer.update tick)
                        model.stage
                        |> Maybe.withDefault model.stage
            }
                ! []

        DismissCardDetailView ->
            { model | cards = ZoomList.unzoom model.cards } ! []


updateAuction : GameCtx AuctionMsg -> AuctionMsg -> Upd AuctionModel
updateAuction { toGameServer } msg model =
    case msg of
        BidButton ->
            model
                ! [ toGameServer <|
                        Api.Bid
                            (case model.auction of
                                Just a ->
                                    Helper.nextBid a

                                Nothing ->
                                    Debug.crash
                                        ("Bid button should be "
                                            ++ "disabled when no card"
                                        )
                            )
                  ]

        ClockUpdated t ->
            model ! []


handleTradeMsg : GameCtx TradeMsg -> TradeMsg -> Upd GameModel
handleTradeMsg { toGameServer, toMsg } msg model =
    case msg of
        Yield ->
            let
                roundAt : Float -> Float -> Int
                roundAt p x =
                    -- [note] only makes sense for 0 <= x <= 1
                    -- mod first to generalize?
                    if x < p then
                        floor x

                    else
                        ceiling x

                yieldWithAvg : Float -> Random.Generator Int
                yieldWithAvg avg =
                    {- Write avg=n+p, where n=floor p.
                       Yield either n or n+1 with probabilities
                       P(n) = 1-p and P(n+1) = p.

                       The average is then:
                       n * (1-p) + (n+1) * p
                       == n+p == avg
                    -}
                    let
                        n =
                            floor avg

                        p =
                            avg - toFloat n
                    in
                    Random.float 0 1
                        |> Random.map
                            (\s ->
                                if s < p then
                                    -- this event has probability p
                                    n + 1

                                else
                                    -- this event has probability 1-p
                                    n
                            )

                -- YIELD RATE CALCULATOR
                effectsModifier =
                    List.foldr
                        (\eff -> Material.map2 (always (*)) eff.yieldRateModifier)
                        (Material.create (always 1))
                        model.effects

                baseYield =
                    List.foldr
                        (\fac -> Material.set fac.fruit fac.number)
                        Material.empty
                        model.factories

                yield : Random.Generator (Material Int)
                yield =
                    let
                        matRandom =
                            Material.map2
                                (always
                                    (\avg c ->
                                        yieldWithAvg avg
                                            -- one yield per factory
                                            |> Random.list c
                                            |> Random.map List.sum
                                    )
                                )
                                effectsModifier
                                baseYield
                    in
                    Random.map4 Material
                        matRandom.blueberry
                        matRandom.tomato
                        matRandom.corn
                        matRandom.purple
            in
            ( model, Random.generate (toMsg << YieldRoll) yield )

        MoveToBasket fruit count ->
            updateIf trade
                (\m model ->
                    case Helper.move fruit count model.inventory m.basket of
                        Nothing ->
                            Debug.crash
                                "+/- buttons should be disabled"

                        Just ( newInv, newBasket ) ->
                            tryUpdate trade
                                (\m -> { m | basket = newBasket } ! [])
                                { model | inventory = newInv }
                )
                model

        EmptyBasket ->
            updateIf trade
                (\m model ->
                    tryUpdate trade
                        (\m -> { m | basket = Material.empty } ! [])
                        { model
                            | inventory =
                                Material.map2 (always (+))
                                    m.basket
                                    model.inventory
                        }
                )
                model

        Shake ->
            tryUpdate trade
                (\m ->
                    m ! [ toGameServer (Api.Trade m.basket) ]
                )
                model

        YieldRoll yield ->
            updateIf trade
                (\_ model ->
                    { model
                        | inventory =
                            Material.map2
                                (always (+))
                                model.inventory
                                yield
                    }
                        ! []
                )
                model


handleAction : Api.Action -> Upd AppModel
handleAction action model =
    case action of
        Api.Welcome name ->
            Game (initGameModel name) ! []

        Api.GameStateChanged stage ->
            tryUpdate game (changeStage stage) model

        Api.Auction seed ->
            tryUpdate (game |> goIn auction)
                (\m ->
                    { m
                        | auction =
                            Just
                                { card = Card.fromSeed seed
                                , highestBid = Nothing
                                , timer = Timer.init (5 * Time.second)
                                }
                    }
                        ! []
                )
                model

        Api.BidUpdated bid winner ->
            tryUpdate (game |> goIn auction)
                (\m ->
                    { m
                        | auction =
                            Maybe.map
                                (\a ->
                                    { a
                                        | highestBid =
                                            Just
                                                { bidder = winner
                                                , bid = bid
                                                }
                                    }
                                )
                                m.auction
                    }
                        ! []
                )
                model

        Api.SetClock ms ->
            tryUpdate game
                (\m ->
                    { m
                        | stage =
                            Lens.update timer
                                (Timer.setTimeLeft
                                    (toFloat ms * Time.millisecond)
                                )
                                m.stage
                                |> Maybe.withDefault m.stage
                    }
                        ! []
                )
                model

        Api.AuctionWon ->
            {- display "You Won!" message -}
            (tryUpdate game << updateIf auction)
                (\m model ->
                    case m.auction of
                        Just a ->
                            { model
                                | -- [tofix] handle too many cards
                                  -- [tofix] append instead of prepend
                                  cards = ZoomList.prepend a.card model.cards
                                , gold =
                                    model.gold
                                        - (case a.highestBid of
                                            Just { bid } ->
                                                bid

                                            Nothing ->
                                                Debug.crash
                                                    "You won for free (???)"
                                          )
                            }
                                ! []

                        Nothing ->
                            model ! []
                )
                model

        Api.EffectActivated cardId author ->
            let
                card =
                    Card.fromSeed cardId

                effect =
                    { name = card.name
                    , author = author
                    , yieldRateModifier = card.yieldRateModifier

                    --[tmp] hard coded
                    , roundsLeft = 2
                    }
            in
            tryUpdate game
                (\m ->
                    { m | effects = effect :: m.effects } ! []
                )
                model

        Api.TradeCompleted mat ->
            tryUpdate (game |> goIn trade)
                (\m -> { m | basket = mat } ! [])
                model

        Api.GameOver winner ->
            model ! []

        Api.PlayerInfoUpdated info ->
            tryUpdate (game |> goIn ready)
                (\m -> { m | playerInfo = info } ! [])
                model


changeStage : StageType -> Upd GameModel
changeStage stage model =
    let
        ( newStage, cmd ) =
            case stage of
                ReadyStageType ->
                    ReadyStage initReadyModel ! []

                AuctionStageType ->
                    AuctionStage initAuctionModel ! []

                TradeStageType ->
                    TradeStage initTradeModel ! []
    in
    ( { model | stage = newStage }
    , cmd
    )



-- HELPER UPDATERS


type alias EffLens submodel model =
    Lens submodel (Eff submodel) model (Eff model)


welcome : EffLens WelcomeModel AppModel
welcome =
    let
        get model =
            case model of
                WelcomeScreen m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( WelcomeScreen m, cmd )
    in
    { get = get, set = set }


game : EffLens GameModel AppModel
game =
    let
        get model =
            case model of
                Game m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( Game m, cmd )
    in
    { get = get, set = set }


ready : EffLens ReadyModel GameModel
ready =
    let
        get model =
            case model.stage of
                ReadyStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( { model | stage = ReadyStage m }, cmd )
    in
    { get = get, set = set }


auction : EffLens AuctionModel GameModel
auction =
    let
        get model =
            case model.stage of
                AuctionStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( { model | stage = AuctionStage m }, cmd )
    in
    { get = get, set = set }


trade : EffLens TradeModel GameModel
trade =
    let
        get model =
            case model.stage of
                TradeStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            Just ( { model | stage = TradeStage m }, cmd )
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



-- HELPERS


baseYieldRate : Material Float
baseYieldRate =
    Material.create (always 1)


totalYieldRate : Material Float -> Material Int -> Material Int
totalYieldRate =
    Material.map3 (always (\a b c -> floor (a * b) * c)) baseYieldRate
