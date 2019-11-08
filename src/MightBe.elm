module MightBe exposing (MightBe(..), map, map2)


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


map2 : (a -> b -> c) -> MightBe a -> MightBe b -> MightBe c
map2 fn mba mbb =
    case mba of
        JustDont ->
            JustDont

        Only valA ->
            case mbb of
                JustDont ->
                    JustDont

                Only valB ->
                    Only (fn valA valB)
