module Helper exposing (arrayRemove, bidIncrement, isErr, isOk, move, nextBid, tryApplyZoomCardEffect, tryApplyZoomCardEffectLocal)

import Api
import Array exposing (Array)
import BaseType exposing (..)
import Material exposing (Fruit, Material)
import Model exposing (..)
import Msg exposing (Msg)
import Server
import ZoomList


bidIncrement : number
bidIncrement =
    5


nextBid : Auction -> Int
nextBid auction =
    case auction.highestBid of
        Just { bid } ->
            bid + bidIncrement

        Nothing ->
            auction.card.startingBid


tryApplyZoomCardEffectLocal : GameModel -> Result String GameModel
tryApplyZoomCardEffectLocal model =
    ZoomList.zoomed model.cards
        |> Result.fromMaybe "No zoomed card"
        |> Result.andThen
            (\card ->
                let
                    removeFromInv : GameModel -> Result String GameModel
                    removeFromInv m =
                        m.inventory
                            |> Material.trySubtract card.resourceCost
                            |> Result.fromMaybe
                                "Not enough resources to activate card."
                            |> Result.map
                                (\inv -> { m | inventory = inv })

                    removeCharge : GameModel -> GameModel
                    removeCharge m =
                        { m
                            | cards =
                                ZoomList.updateZoomed
                                    (\c ->
                                        let
                                            chargeLeft =
                                                BaseType.add card.charge -1
                                        in
                                        if chargeLeft == Finite 0 then
                                            Nothing

                                        else
                                            Just
                                                { c | charge = chargeLeft }
                                    )
                                    m.cards
                        }

                    closeDetailView : GameModel -> GameModel
                    closeDetailView m =
                        { m | cards = ZoomList.unzoom m.cards }
                in
                model
                    |> removeFromInv
                    |> Result.map
                        (removeCharge
                            >> closeDetailView
                        )
            )


tryApplyZoomCardEffect :
    Server.SendToServer
    -> GameModel
    -> Result String ( GameModel, Cmd Msg )
tryApplyZoomCardEffect toServer model =
    ZoomList.zoomed model.cards
        |> Result.fromMaybe "No zoomed card"
        |> Result.andThen
            (\card ->
                tryApplyZoomCardEffectLocal model
                    |> Result.map
                        (\m ->
                            let
                                send : Cmd Msg
                                send =
                                    toServer (Api.ActivateEffect card.id_)
                            in
                            ( m, send )
                        )
            )


arrayRemove : Int -> Array a -> Maybe (Array a)
arrayRemove i array =
    Array.get i array
        |> Maybe.map
            (always
                (Array.append (Array.slice 0 i array)
                    (Array.slice (i + 1) (Array.length array) array)
                )
            )


move :
    Fruit
    -> Int
    -> Material Int
    -> Material Int
    -> Maybe ( Material Int, Material Int )
move fruit count mat1 mat2 =
    let
        newMat1 =
            Material.update fruit
                (flip (-) count)
                mat1

        newMat2 =
            Material.update fruit
                ((+) count)
                mat2
    in
    if
        Material.lookup fruit newMat1
            < 0
            || Material.lookup fruit newMat2
            < 0
    then
        Nothing

    else
        Just ( newMat1, newMat2 )


isOk : Result e a -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


isErr : Result e a -> Bool
isErr =
    not << isOk
