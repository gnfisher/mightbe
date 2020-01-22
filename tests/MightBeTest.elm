module MightBeTest exposing (suite)

import Debug
import Expect
import MightBe exposing (MightBe(..), andMap, andThen, map, map2)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "MightBe"
        [ describe "MightBe is a Functor!"
            [ test "Must preserve identity morphisms" <|
                \_ ->
                    Some 1
                        |> MightBe.map identity
                        |> Expect.equal (Some 1)
            , test "preserve composition of morphims" <|
                \_ ->
                    Some 2
                        |> MightBe.map addOne
                        |> MightBe.map double
                        |> Expect.equal (Some 2 |> MightBe.map (double << addOne))
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

        -- Some 3 |> andThen (Some << double) == (Some << double 3)
        -- Some 3 |> andThen Some == Some 3
        , describe "MightBe.andThen aka Monadic Laws"
            [ test "Right Identity" <|
                \_ ->
                    Some 1
                        |> MightBe.andThen mightDouble
                        |> Expect.equal (mightDouble 1)
            , test "Left Identity" <|
                \_ ->
                    Some 1
                        |> MightBe.andThen Some
                        |> Expect.equal (Some 1)
            , test "Associativity" <|
                --(m >>= f) >>= g = m >>= (\x -> f x >>= g)
                -- m = Some 1
                -- f = mightDouble
                -- g = mightAddOne
                \_ ->
                    Some 1
                        |> andThen mightDouble
                        |> andThen mightAddOne
                        |> Expect.equal
                            (Some 1
                                |> MightBe.andThen
                                    (\x ->
                                        mightDouble x
                                            |> MightBe.andThen mightAddOne
                                    )
                            )
            ]
        , describe "MightBe.andMap aka Applicative laws "
            [ test "Identity" <|
                \_ ->
                    Some identity
                        |> MightBe.andMap (Some 1)
                        |> Expect.equal (Some 1)
            , test "Homomorphism" <|
                \_ ->
                    Some double
                        |> MightBe.andMap (Some 1)
                        |> Expect.equal (Some (double 1))
            , test "Interchange" <|
                -- u <*> pure y = pure ($ y) <*> u
                -- u = (Some double)
                -- y = 1
                \_ ->
                    Some double
                        |> andMap (Some 1)
                        |> Expect.equal (Some (\fn -> fn 1) |> andMap (Some double))
            , test "Composition" <|
                --pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
                -- u = (Some double)
                -- v = (Some addOne)
                -- w = (Some 1)
                \_ ->
                    Some (<<)
                        |> MightBe.andMap (Some double)
                        |> MightBe.andMap (Some addOne)
                        |> MightBe.andMap (Some 1)
                        |> Expect.equal
                            (Some double
                                |> MightBe.andMap (Some addOne |> MightBe.andMap (Some 1))
                            )
            ]
        ]


addOne : Int -> Int
addOne a =
    a + 1


double : Int -> Int
double a =
    a * 2


mightDouble : Int -> MightBe Int
mightDouble n =
    Some (double n)


mightAddOne : Int -> MightBe Int
mightAddOne n =
    Some (addOne n)
