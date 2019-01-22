module View exposing (view)

import BaseType exposing (..)
import Helper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lens
import Material exposing (Material)
import Model exposing (..)
import Msg exposing (..)
import Time
import Timer


view : Model -> Html Msg
view model =
    div [ class "overview" ] <|
        List.singleton <|
            Html.map AppMsg <|
                case model.app of
                    JoinGameScreen model ->
                        Html.map JoinGameMsg (joinGameView model)

                    GameScreen model ->
                        Html.map GameMsg (gameView model)


joinGameView : JoinGameModel -> Html JoinGameMsg
joinGameView model =
    div [ class "welcome" ]
        [ div [] []
        , div [ class "box" ]
            [ div [ class "box-text" ]
                [ text "Type the name of the game to join:" ]
            , input
                [ placeholder "Game Name"
                , onInput GameNameInputChange
                ]
                []
            , button
                [ class "box-button"
                , onClick JoinGameButton
                ]
                [ text "Join Game" ]
            ]
        ]


gameView : GameModel -> Html GameMsg
gameView model =
    div [ class "view" ] <|
        List.concat
            [ [ topBar model
              , div [ class "active-state" ]
                    [ case model.stage of
                        WaitStage m ->
                            Html.map WaitMsg (waitView m)

                        SiteSelectionStage m ->
                            Html.map SiteSelectionMsg (siteSelectionView m)

                        SiteVisitStage m ->
                            Html.map SiteVisitMsg (siteVisitView m)

                        GameOverStage ->
                            gameOverView
                    ]
              ]
            , [ div [ class "tray" ]
                    [ basketView model
                    , inventoryView model.inventory
                    ]
              ]
            ]


icon : String -> String -> Html GameMsg
icon class_name icon_name =
    i [ class ("material-icons " ++ class_name) ] [ text icon_name ]


topBar : GameModel -> Html GameMsg
topBar model =
    div [ class "heading" ]
        [ icon "link-icon" "link"
        , div [ class "game-title" ] [ text model.gameName ]
        , div [ class "timer" ]
            (List.concat
                [ case
                    Lens.get timer model.stage
                  of
                    Just timer ->
                        [ div [ class "timer-text" ]
                            [ text
                                (toString
                                    << round
                                    << Time.inSeconds
                                    << Timer.timeLeft
                                 <|
                                    timer
                                )
                            ]
                        , icon "timer-icon" "timer"
                        ]

                    Nothing ->
                        []
                ]
            )
        ]


inventoryView : Material Int -> Html GameMsg
inventoryView inv =
    div [ class "inventory" ]
        (Material.values
            (Material.map
                (\resource count ->
                    div [ class ("inventory-item " ++ toString resource) ]
                        [ text (toString count) ]
                )
                inv
            )
        )


basketView : GameModel -> Html GameMsg
basketView { inventory, basket } =
    let
        setDisplayStyle display =
            div << (::) (style [ ( "display", display ) ])

        table =
            setDisplayStyle "table"

        row =
            setDisplayStyle "table-row"

        cell =
            setDisplayStyle "table-cell"
    in
    table [] <|
        List.concat
            [ [ row [] <|
                    List.map (cell [] << List.singleton) <|
                        List.concat
                            [ [ text "Basket:" ]
                            , List.map
                                (text
                                    << toString
                                    << flip Material.lookup basket
                                )
                                Material.allResources
                            , [ button [ onClick EmptyBasket ]
                                    [ text "Empty" ]
                              ]
                            ]
              , row
                    []
                <|
                    List.map (cell [] << List.singleton) <|
                        List.concat
                            [ [ text "" ]
                            , List.map
                                (\rsr ->
                                    button
                                        [ onClick (MoveToBasket rsr 1)
                                        , disabled
                                            (Nothing
                                                == Helper.move rsr
                                                    1
                                                    inventory
                                                    basket
                                            )
                                        ]
                                        [ text "^" ]
                                )
                                Material.allResources
                            ]
              , row
                    []
                <|
                    List.map (cell [] << List.singleton) <|
                        List.concat
                            [ [ text "" ]
                            , List.map
                                (\rsr ->
                                    button
                                        [ onClick (MoveToBasket rsr -1)
                                        , disabled
                                            (Nothing
                                                == Helper.move rsr
                                                    -1
                                                    inventory
                                                    basket
                                            )
                                        ]
                                        [ text "v" ]
                                )
                                Material.allResources
                            ]
              , row
                    []
                <|
                    List.map (cell [] << List.singleton) <|
                        List.concat
                            [ [ text "Inv:" ]
                            , List.map
                                (text
                                    << toString
                                    << flip Material.lookup inventory
                                )
                                Material.allResources
                            ]
              ]
            ]


waitView : WaitModel -> Html WaitMsg
waitView m =
    div []
        [ div [ class "box" ]
            [ div [ class "box-text" ] [ text "Set your name:" ]
            , input [ placeholder "Anonymous", onInput NameInputChange ] []
            , button [ class "box-button", onClick (Ready True) ]
                [ text "Ready" ]
            ]
        , div [ class "ready-status" ]
            [ div [ class "box-text" ] [ text "Waiting for players..." ]
            , div [ class "player-statuses" ] <|
                List.map
                    (\a ->
                        div [ class "player-status" ]
                            [ text a.name
                            , readyOrWaitingIcon a.ready
                            ]
                    )
                    m.playerInfo
            ]
        ]


readyOrWaitingIcon : Bool -> Html WaitMsg
readyOrWaitingIcon state =
    if state then
        i [ class "material-icons ready-icon ready" ] [ text "check" ]

    else
        i [ class "material-icons ready-icon waiting" ] [ text "timelapse" ]


siteSelectionView : SiteSelectionModel -> Html SiteSelectionMsg
siteSelectionView m =
    let
        siteButton site =
            button [ onClick (SiteSelected site) ]
                [ text <| siteToString site ]
    in
    div [] <|
        List.concat
            [ List.map siteButton allSites
            ]


siteVisitView : SiteVisitModel -> Html SiteVisitMsg
siteVisitView m =
    div [] <|
        case m.event of
            Just e ->
                List.concat <|
                    [ [ h1 [] [ text e.title ]
                      , text e.description
                      , div [] <|
                            List.concat
                                [ case e.okButton of
                                    Just txt ->
                                        [ button [ onClick OkButton ]
                                            [ text txt ]
                                        ]

                                    Nothing ->
                                        []
                                , case e.spendButton of
                                    Just rsr ->
                                        [ button [ onClick UseResourceButton ]
                                            -- [todo] let users choose amount
                                            [ text <| "Use a " ++ toString rsr ]
                                        ]

                                    Nothing ->
                                        []
                                , case e.actionButton of
                                    Just ab ->
                                        [ button [ onClick ActionButton ]
                                            [ text <|
                                                ab.actionButtonText
                                                    ++ " (Uses "
                                                    ++ toString ab.actionButtonResourceAmount
                                                    ++ toString ab.actionButtonResource
                                                    ++ ")"
                                            ]
                                        ]

                                    Nothing ->
                                        []
                                ]
                      ]
                    ]

            Nothing ->
                [ text "Nothing seems to be happening..." ]


gameOverView : Html GameMsg
gameOverView =
    -- [todo] display winner, stats, etc
    div [] [ text "Game Over!" ]
