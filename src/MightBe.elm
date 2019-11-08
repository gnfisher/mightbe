module MightBe exposing
    ( MightBe(..)
    , andMap
    , andMapHard
    , andThen
    , map
    , map2
    )


type MightBe a
    = Some a
    | None


map : (a -> b) -> MightBe a -> MightBe b
map fn mightBe =
    case mightBe of
        Some val ->
            Some (fn val)

        None ->
            None


map2 : (a -> b -> c) -> MightBe a -> MightBe b -> MightBe c
map2 fn mba mbb =
    case mba of
        None ->
            None

        Some valA ->
            case mbb of
                None ->
                    None

                Some valB ->
                    Some (fn valA valB)


andMap : MightBe a -> MightBe (a -> b) -> MightBe b
andMap mba mbfn =
    map2 (\a fn -> fn a) mba mbfn


andMapHard : MightBe a -> MightBe (a -> b) -> MightBe b
andMapHard mba mbfn =
    case mba of
        None ->
            None

        Some valA ->
            case mbfn of
                None ->
                    None

                Some valFn ->
                    Some (valFn valA)


andThen : (a -> MightBe b) -> MightBe a -> MightBe b
andThen fn mba =
    case mba of
        None ->
            None

        Some val ->
            fn val
