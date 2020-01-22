module MightBe exposing
    ( MightBe(..)
    , map
    )

import Debug


type MightBe a
    = Some a
    | None


map : (a -> b) -> MightBe a -> MightBe b
map fn mightBe =
    Debug.todo "Make this test pass by implementing map"
