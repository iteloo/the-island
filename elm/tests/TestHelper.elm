module TestHelper exposing (..)

import Helper
import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, array)
import Test exposing (..)


suite : Test
suite =
    describe "The Helper module"
        [ describe "arrayRemove"
            [ test "removing an element in the middle" <|
                \_ ->
                    Expect.equal
                        (Helper.arrayRemove 2 <| Array.fromList [ 1, 2, 3, 4 ])
                        (Just <| Array.fromList [ 1, 2, 4 ])
            , fuzz
                (Fuzz.map2 ((,)) (array string) (Fuzz.intRange -3 10))
                "reduces length by 1"
              <|
                \( array, i ) ->
                    let
                        lenRemoved =
                            Maybe.map Array.length <|
                                Helper.arrayRemove i array

                        len =
                            Array.length array
                    in
                        Expect.equal lenRemoved <|
                            if i < 0 then
                                Nothing
                            else if i < len then
                                Just (len - 1)
                            else
                                Nothing
            ]
        ]
