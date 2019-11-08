module MightBeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MightBe exposing (MightBe(..), andMap, andThen, map, map2)
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
            , test "returns JustDont when passing in JustDont" <|
                \_ ->
                    JustDont
                        |> MightBe.map (\val -> val * 2)
                        |> Expect.equal JustDont
            ]
        , describe "MightBe.map2"
            [ test "applies function to combine two MightBes" <|
                \_ ->
                    MightBe.map2 (\a b -> a + b) (Only 1) (Only 2)
                        |> Expect.equal (Only 3)
            , test "If one is JustDont it returns JustDont" <|
                \_ ->
                    MightBe.map2 (\a b -> a + b) (Only 1) JustDont
                        |> Expect.equal JustDont
            ]
        , describe "MightBe.andMap"
            [ test "it applies the wrapped fn to the first wrapped value" <|
                \_ ->
                    MightBe.andMap (Only 1) (Only (\a -> a * 2))
                        |> Expect.equal (Only 2)
            , test "it works with JustDont" <|
                \_ ->
                    MightBe.andMap JustDont (Only (\a -> a * 2))
                        |> Expect.equal JustDont
            ]
        , describe "MightBe.andThen"
            [ test "it applies the function without re-wrapping the value" <|
                \_ ->
                    Only 1
                        |> MightBe.andThen (\a -> Only (a * 2))
                        |> Expect.equal (Only 2)
            , test "it works with JustDont case" <|
                \_ ->
                    JustDont
                        |> MightBe.andThen (\a -> Only (a * 2))
                        |> Expect.equal JustDont
            ]
        ]
