module Api exposing
    ( Action(..)
    , ServerAction(..)
    , decodeMessage
    , encodeToMessage
    )

import BaseType exposing (..)
import Json.Decode as D
import Json.Encode as E
import Material exposing (Fruit, Material)


type Action
    = Welcome String
    | GameStateChanged StageType
    | SetClock Int
    | Auction CardSeed
    | BidUpdated Int String
    | AuctionWon
    | EffectActivated CardSeed String
    | TradeCompleted (Material Int)
    | GameOver String
    | PlayerInfoUpdated (List PlayerInfo)


decodeMessage : String -> Result String Action
decodeMessage =
    D.decodeString action


action : D.Decoder Action
action =
    D.field "action" D.string |> D.andThen actionHelp


actionHelp : String -> D.Decoder Action
actionHelp a =
    case a of
        "game_state_changed" ->
            D.map GameStateChanged <|
                (D.field "new_state" D.string
                    |> D.andThen
                        (\s ->
                            case s of
                                "ready" ->
                                    D.succeed ReadyStageType

                                "auction" ->
                                    D.succeed AuctionStageType

                                "trade" ->
                                    D.succeed TradeStageType

                                _ ->
                                    D.fail "Unrecognized stage name"
                        )
                )

        "welcome" ->
            D.map Welcome <|
                D.field "game" D.string

        "set_clock" ->
            D.map SetClock
                (D.field "time" D.int)

        "player_info_updated" ->
            D.map PlayerInfoUpdated
                (D.field "info"
                    (D.list
                        (D.map2 PlayerInfo
                            (D.field "name" D.string)
                            (D.field "ready" D.bool)
                        )
                    )
                )

        "auction_seed" ->
            D.map Auction <|
                D.field "seed" D.int

        "bid_updated" ->
            D.map2 BidUpdated
                (D.field "bid" D.int)
                (D.field "winner" D.string)

        "auction_won" ->
            D.succeed AuctionWon

        "effect_activated" ->
            D.map2 EffectActivated
                (D.field "card_id" D.int)
                --[note] doesn't sync up with name change
                (D.field "author" D.string)

        "trade_completed" ->
            D.map TradeCompleted <|
                D.field "materials"
                    (D.string
                        |> D.andThen
                            (D.decodeString (material D.int)
                                >> (\r ->
                                        case r of
                                            Ok mat ->
                                                D.succeed mat

                                            Err e ->
                                                D.fail e
                                   )
                            )
                    )

        "game_over" ->
            D.map GameOver <|
                D.field "winner" D.string

        _ ->
            D.fail ("Received unrecognized action from server: " ++ a)


fruit : D.Decoder Fruit
fruit =
    D.string
        |> D.andThen
            (\s ->
                case Material.fruitFromString s of
                    Just fruit ->
                        D.succeed fruit

                    Nothing ->
                        D.fail "Unrecognized fruit name"
            )


material : D.Decoder a -> D.Decoder (Material a)
material a =
    D.map4 Material
        (D.field "blueberry" a)
        (D.field "tomato" a)
        (D.field "corn" a)
        (D.field "purple" a)


type ServerAction
    = JoinGame String
    | Ready Bool
    | Bid Int
    | SetName String
    | Sell Fruit Int
    | Trade (Material Int)
    | ActivateCard CardSeed
    | ActivateEffect CardSeed


encodeToMessage : ServerAction -> String
encodeToMessage =
    E.encode 0 << encodeServerAction


encodeServerAction : ServerAction -> E.Value
encodeServerAction a =
    let
        ( actionStr, values ) =
            case a of
                JoinGame gameId ->
                    ( "join_game"
                    , [ ( "name", E.string gameId )
                      ]
                    )

                Ready state ->
                    ( "ready"
                    , [ ( "ready", E.bool state )
                      ]
                    )

                Bid x ->
                    ( "bid"
                    , [ ( "amount", E.int x )
                      ]
                    )

                SetName name ->
                    ( "set_name", [ ( "name", E.string name ) ] )

                Sell type_ quantity ->
                    ( "sell"
                    , [ ( "type", encodeFruit type_ )
                      , ( "quantity", E.int quantity )
                      ]
                    )

                Trade mat ->
                    ( "trade"
                    , [ ( "materials"
                        , E.string
                            (E.encode 0 (encodeMaterial E.int mat))
                        )
                      ]
                    )

                {- [todo] Finish implementing -}
                ActivateCard seed ->
                    ( "activate_card", [] )

                ActivateEffect id_ ->
                    ( "activate_effect"
                    , [ ( "card_id", E.int id_ )
                      ]
                    )
    in
    E.object <|
        [ ( "action", E.string actionStr )
        ]
            ++ values


encodeFruit : Fruit -> E.Value
encodeFruit =
    toString >> String.toLower >> E.string


encodeMaterial : (a -> E.Value) -> Material a -> E.Value
encodeMaterial a =
    E.object
        << Material.fold
            (\fr v -> (::) ( String.toLower (toString fr), a v ))
            []
