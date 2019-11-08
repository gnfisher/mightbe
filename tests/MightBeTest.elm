module MightBeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (MightBe(..))
import Test exposing (..)


suite : Test
suite =
    describe "MightBe"
        [ describe "MightBe.map"
            [ test "applies the function to the wrapped value in MightBe" <|
                \_ ->
                    Only 1
                        |> MightBe.map (\val -> val * 2)
                        |> Expect.equal (Only 4)
            ]
        ]
