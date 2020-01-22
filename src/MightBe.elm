module MightBe exposing
    ( MightBe(..)
    , andMap
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


andThen : (a -> MightBe b) -> MightBe a -> MightBe b
andThen fn mba =
    case mba of
        None ->
            None

        Some val ->
            fn val


andMap : MightBe a -> MightBe (a -> b) -> MightBe b
andMap mba mbfn =
    case mba of
        None ->
            None

        Some valA ->
            case mbfn of
                None ->
                    None

                Some valFn ->
                    Some (valFn valA)


map2 : (a -> b -> c) -> MightBe a -> MightBe b -> MightBe c
map2 fn mba mbb =
    Some fn
        |> andMap mba
        |> andMap mbb
