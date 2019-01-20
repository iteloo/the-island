module TestZoomList exposing (..)

import ZoomList exposing (ZoomList)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


zoomList : Fuzzer a -> Fuzzer (ZoomList a)
zoomList =
    list
        >> Fuzz.andThen
            (\l ->
                {- [note] for some reason, the Fuzz library doens't let me
                   set the range dynamically using the length of the list, so
                   for now we hardcode a number and return an unzoomed list
                   if the index goes out of bound
                -}
                Fuzz.intRange 0 10
                    |> Fuzz.map
                        (\index ->
                            case
                                ZoomList.fromList l
                                    |> ZoomList.jumpTo index
                            of
                                Nothing ->
                                    ZoomList.fromList l

                                Just zl ->
                                    zl
                        )
            )


suite : Test
suite =
    describe "The ZoomList module"
        [ describe "prev"
            [ fuzz (zoomList string)
                "length should stay constant"
              <|
                \zoomList ->
                    case ZoomList.prev zoomList of
                        Nothing ->
                            Expect.pass

                        Just prev ->
                            Expect.equal
                                (ZoomList.length prev)
                                (ZoomList.length zoomList)
            , fuzz (zoomList string)
                "underlying list should stay the same"
              <|
                \zoomList ->
                    case ZoomList.prev zoomList of
                        Nothing ->
                            Expect.pass

                        Just prev ->
                            Expect.equal
                                (ZoomList.toList prev)
                                (ZoomList.toList zoomList)
            ]
        , describe "foldr"
            [ fuzz (zoomList string)
                "recreating a list should match underlying list"
              <|
                \zoomList ->
                    let
                        list =
                            ZoomList.foldr ((::)) ((::)) [] zoomList
                    in
                        Expect.equal list (ZoomList.toList zoomList)
            ]
        ]
