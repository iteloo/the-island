module Api exposing
    ( Action(..)
    , EventMessage
    , EventResponseMessage
    , ServerAction(..)
    , decodeMessage
    , encodeToMessage
    )

import BaseType exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Material exposing (Material, Resource)


type Action
    = Welcome String
    | GameStateChanged StageType
    | SetClock Int
    | PlayerInfoUpdated (List PlayerInfo)
    | TradeCompleted (Material Int)
    | Event EventMessage
    | GameOver String


type alias EventMessage =
    { messageId : Int
    , title : String
    , description : String
    , okButtonText : Maybe String
    , usedResourceName : Maybe String
    , actionButton : Maybe ActionButtonInfo
    }


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
                                -- [todo] change to "waiting"?
                                "ready" ->
                                    D.succeed WaitStageType

                                "site_selection" ->
                                    D.succeed SiteSelectionStageType

                                "site_visit" ->
                                    D.succeed SiteVisitStageType

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

        "event" ->
            -- [todo] add better logic
            D.succeed EventMessage
                |> D.required "message_id" D.int
                |> D.required "title" D.string
                |> D.required "description" D.string
                |> D.custom okButton
                |> D.custom usedResource
                |> D.custom actionButton
                |> D.map Event

        "game_over" ->
            D.map GameOver <|
                D.field "winner" D.string

        _ ->
            D.fail ("Received unrecognized action from server: " ++ a)


okButton : D.Decoder (Maybe String)
okButton =
    D.field "has_ok" D.bool
        |> D.andThen
            (\hasOk ->
                if hasOk then
                    D.succeed Just
                        |> D.required "ok_button_text" D.string

                else
                    D.succeed Nothing
            )


usedResource : D.Decoder (Maybe String)
usedResource =
    D.field "uses_resource" D.bool
        |> D.andThen
            (\usesResource ->
                if usesResource then
                    D.succeed Just
                        |> D.required "used_resource_name" D.string

                else
                    D.succeed Nothing
            )


actionButton : D.Decoder (Maybe ActionButtonInfo)
actionButton =
    D.field "has_action_button" D.bool
        |> D.andThen
            (\hasActionButton ->
                if hasActionButton then
                    D.succeed ActionButtonInfo
                        |> D.required "action_button_name" D.string
                        |> D.required "action_button_resource_allocation"
                            D.string
                        |> D.map Just

                else
                    D.succeed Nothing
            )


resource : D.Decoder Resource
resource =
    D.string
        |> D.andThen
            (\s ->
                case Material.resourceFromString s of
                    Just resource ->
                        D.succeed resource

                    Nothing ->
                        D.fail "Unrecognized resource name"
            )


material : D.Decoder a -> D.Decoder (Material a)
material a =
    D.map4 Material
        (D.field "log" a)
        (D.field "food" a)
        (D.field "bandage" a)
        (D.field "bullet" a)


type ServerAction
    = JoinGame String
    | Ready Bool
    | SetName String
    | Trade (Material Int)
    | EventResponse EventResponseMessage


type alias EventResponseMessage =
    { messageId : Int
    , clickedOk : Bool
    , clickedAction : Bool
    , resourceAmount : Int
    }


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

                SetName name ->
                    ( "set_name", [ ( "name", E.string name ) ] )

                Trade mat ->
                    ( "trade"
                    , [ ( "materials"
                        , E.string
                            (E.encode 0 (encodeMaterial E.int mat))
                        )
                      ]
                    )

                EventResponse response ->
                    ( "event_response"
                    , [ ( "message_id", E.int response.messageId )
                      , ( "clicked_ok", E.bool response.clickedOk )
                      , ( "clicked_action", E.bool response.clickedAction )
                      , ( "resource_amount", E.int response.resourceAmount )
                      ]
                    )
    in
    E.object <|
        [ ( "action", E.string actionStr )
        ]
            ++ values


encodeResource : Resource -> E.Value
encodeResource =
    toString >> String.toLower >> E.string


encodeMaterial : (a -> E.Value) -> Material a -> E.Value
encodeMaterial a =
    E.object
        << Material.fold
            (\rsr v -> (::) ( String.toLower (toString rsr), a v ))
            []
