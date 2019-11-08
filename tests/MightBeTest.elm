module MightBeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MightBe exposing (MightBe(..), map)
import Test exposing (..)


suite : Test
suite =
    describe "MightBe"
        [ describe "MightBe.map"
            [ test "applies the function to the wrapped value in MightBe" <|
                \_ ->
                    Only 2
                        |> MightBe.map (\val -> val * 2)
                        |> Expect.equal (Only 4)
            ]
        ]
