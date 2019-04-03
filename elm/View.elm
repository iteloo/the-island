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
    div [ class "game-view" ] <|
        List.concat
            [ [ topBar model
              , div [ class "active-state" ]
                    [ case model.stage of
                        WaitStage m ->
                            Html.map WaitMsg (waitView m)

                        SiteSelectionStage m ->
                            Html.map SiteSelectionMsg (siteSelectionView m)

                        SiteVisitStage m ->
                            Html.map SiteVisitMsg (siteVisitView model m)

                        GameOverStage ->
                            gameOverView
                    ]
              ]
            , [ div [ class "tray" ]
                    [ basketView model ]
              ]
            ]


icon : String -> String -> Html msg
icon class_name icon_name =
    i [ class ("material-icons " ++ class_name) ] [ text icon_name ]


imgIcon : String -> String -> Html msg
imgIcon className imgName =
    img
        [ class ("img-icon " ++ className)
        , src imgName
        ]
        []


topBar : GameModel -> Html GameMsg
topBar model =
    div [ class "topbar" ] <|
        List.concat
            [ [ icon "link-icon" "link" ]
            , case model.stage of
                -- [hack] not the cleanest
                WaitStage _ ->
                    []

                _ ->
                    [ status model ]
            , [ div [ class "game-title" ] [ text model.gameName ] ]
            , maybeA timerView model.timer
            ]


timerView timer =
    div [ class "timer" ]
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


status { health, antihunger } =
    let
        intBisect maxValue x =
            let
                ceilX =
                    ceiling x
                        |> Basics.min maxValue
                        |> Basics.max 0
            in
            ( ceilX, maxValue - ceilX )

        ( healthInt, emptyHealthInt ) =
            intBisect maxHealth health

        ( antihungerInt, emptyAntihungerInt ) =
            intBisect maxAntihunger antihunger
    in
    div [ class "status" ] <|
        List.concat
            [ List.repeat healthInt <|
                icon "health-icon" "favorite"
            , List.repeat emptyHealthInt <|
                icon "empty-health-icon" "favorite_border"
            , List.repeat antihungerInt <|
                imgIcon "antihunger-icon" "assets/carrot.png"
            , List.repeat emptyAntihungerInt <|
                imgIcon "empty-antihunger-icon" "assets/carrot-transparent.png"
            ]


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

        resourceIconFiles : Material String
        resourceIconFiles =
            { log = "log.png"
            , food = "carrot.png"
            , bandage = "bandage.png"
            , bullet = "bullet.png"
            }
                |> Material.map (\_ -> (++) "assets/")

        resourceView modifier resourceCounts =
            Material.values <|
                Material.map2
                    (\rsr count iconFile ->
                        let
                            canMove =
                                Helper.move rsr
                                    modifier
                                    inventory
                                    basket
                                    /= Nothing
                        in
                        span
                            (List.concat
                                [ if canMove then
                                    [ onClick (MoveToBasket rsr modifier) ]

                                  else
                                    []
                                ]
                            )
                            [ imgIcon (Material.shorthand rsr)
                                iconFile
                            , text <| toString count
                            ]
                    )
                    resourceCounts
                    resourceIconFiles
    in
    table [] <|
        List.concat
            [ [ row [] <|
                    List.map (cell [] << List.singleton) <|
                        List.concat
                            [ [ text "Basket:" ]
                            , resourceView -1 basket
                            , [ button [ onClick EmptyBasket ]
                                    [ text "Empty" ]
                              , button [ onClick TradeButton ]
                                    [ text "Trade" ]
                              ]
                            ]
              , row [] <|
                    List.map (cell [] << List.singleton) <|
                        List.concat
                            [ [ text "Bag:" ]
                            , resourceView 1 inventory
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
        siteButton mSiteSelected site =
            a
                [ class "site-button"
                , class (siteToString site ++ "-button")
                , case mSiteSelected |> Maybe.map ((==) site) of
                    Just True ->
                        class "site-button-selected"

                    _ ->
                        class ""
                , onClick (SiteSelected site)
                ]
                [ text <| siteToString site ]
    in
    div [] <|
        List.concat
            [ [ div []
                    [ text """It's getting dark... Use this time to discuss
                     the day's finding with the group and plan the next day."""
                    ]
              , div [] [ text """Remember, your
             goal is to leave the island – tap the icon on the top-right corner
             of the screen to see how to do that.""" ]
              , div []
                    [ text """Select the site you'd like to visit tomorrow.
                      The night will be over when everyone's made a selection:
                        """ ]
              , div [ class "site-buttons-container" ] <|
                    List.map (siteButton m.siteSelected) allSites
              ]
            ]


siteVisitView : GameModel -> SiteVisitModel -> Html SiteVisitMsg
siteVisitView { inventory } m =
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
                                        [ text <|
                                            "Use "
                                                ++ toString e.resourceAmountSelected
                                                ++ toString rsr
                                        , button
                                            [ onClick AddResourceSpendButton
                                            , disabled <|
                                                e.resourceAmountSelected
                                                    >= Material.lookup rsr inventory
                                            ]
                                            [ text <| "+" ]
                                        , button
                                            [ onClick RemoveResourceSpendButton
                                            , disabled <| e.resourceAmountSelected <= 0
                                            ]
                                            [ text <| "-" ]
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


maybeA : (b -> a) -> Maybe b -> List a
maybeA f mb =
    case mb of
        Nothing ->
            []

        Just b ->
            [ f b ]
