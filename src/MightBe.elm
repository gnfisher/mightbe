module MightBe exposing (MightBe(..), map)


type MightBe a
    = Only a
    | JustDont


map : (a -> b) -> MightBe a -> MightBe b
map fn mightBe =
    case mightBe of
        Only val ->
            Only (fn val)

        JustDont ->
            JustDont
