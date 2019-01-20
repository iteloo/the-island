module Helper exposing
    ( arrayRemove
    , isErr
    , isOk
    , move
    )

import Api
import Array exposing (Array)
import BaseType exposing (..)
import Material exposing (Material, Resource)
import Model exposing (..)
import Msg exposing (Msg)
import Server


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
    Resource
    -> Int
    -> Material Int
    -> Material Int
    -> Maybe ( Material Int, Material Int )
move resource count mat1 mat2 =
    let
        newMat1 =
            Material.update resource
                (flip (-) count)
                mat1

        newMat2 =
            Material.update resource
                ((+) count)
                mat2
    in
    if
        Material.lookup resource newMat1
            < 0
            || Material.lookup resource newMat2
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
