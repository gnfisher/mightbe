module MightBeTest exposing (suite)

import Debug
import Expect
import MightBe exposing (MightBe(..), map)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "MightBe is a Functor, so it must have a map function"
        [ describe "A 'Lawful' map must do two things"
            [ test "1. If you map identity, you get back what you put in" <|
                \_ ->
                    Some 1
                        |> MightBe.map identity
                        |> Expect.equal (Some 1)
            , test "2. Mapping a function over a Functor that contains a value, is the same as creating a Functor with the result of applying that function to that value" <|
                \_ ->
                    MightBe.map addOne (Some 1)
                        |> Expect.equal (Some (addOne 1))
            , test "3. Mapping a composed function over a Functor, is the same as mapping the functions sequentially" <|
                \_ ->
                    let
                        composedFunctions =
                            double << addOne
                    in
                    Some 2
                        |> MightBe.map addOne
                        |> MightBe.map double
                        |> Expect.equal (Some 2 |> MightBe.map composedFunctions)
            ]
        ]


addOne : Int -> Int
addOne a =
    a + 1


double : Int -> Int
double a =
    a * 2
