module MightBeTest exposing (suite)

import Expect
import MightBe exposing (MightBe(..), andMap, andThen, map, map2)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "MightBe"
        [ describe "MightBe.map"
            [ test "applies the function to the wrapped value in MightBe" <|
                \_ ->
                    Some 2
                        |> MightBe.map (\val -> val * 2)
                        |> Expect.equal (Some 4)
            , test "returns None when passing in None" <|
                \_ ->
                    None
                        |> MightBe.map (\val -> val * 2)
                        |> Expect.equal None
            ]
        , describe "MightBe.map2"
            [ test "applies function to combine two MightBes" <|
                \_ ->
                    MightBe.map2 (\a b -> a + b) (Some 1) (Some 2)
                        |> Expect.equal (Some 3)
            , test "If one is None it returns None" <|
                \_ ->
                    MightBe.map2 (\a b -> a + b) (Some 1) None
                        |> Expect.equal None
            ]
        , describe "MightBe.andMap"
            [ test "it applies the wrapped fn to the first wrapped value" <|
                \_ ->
                    MightBe.andMap (Some 1) (Some (\a -> a * 2))
                        |> Expect.equal (Some 2)
            , test "it works with None" <|
                \_ ->
                    MightBe.andMap None (Some (\a -> a * 2))
                        |> Expect.equal None
            ]
        , describe "MightBe.andThen"
            [ test "it applies the function without re-wrapping the value" <|
                \_ ->
                    Some 1
                        |> MightBe.andThen (\a -> Some (a * 2))
                        |> Expect.equal (Some 2)
            , test "it works with None case" <|
                \_ ->
                    None
                        |> MightBe.andThen (\a -> Some (a * 2))
                        |> Expect.equal None
            ]
        ]
