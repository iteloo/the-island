module ZoomList
    exposing
        ( ZoomList
        , empty
        , fromList
        , toList
        , zoomed
        , length
        , prepend
        , next
        , prev
        , jumpTo
        , unzoom
        , updateZoomed
        , map
        , foldr
        )

{-| The `ZoomList` data type models a list with an optional
focus on one of its elements. The constructors are not exported.
-}


type ZoomList a
    = UnZoomed (List a)
    | -- [note] `Zooming before focus after`
      Zooming (List a) a (List a)


empty : ZoomList a
empty =
    fromList []


{-| Create an unzoomed zoomlist from an ordinary list
-}
fromList : List a -> ZoomList a
fromList =
    UnZoomed


toList : ZoomList a -> List a
toList zlist =
    case zlist of
        UnZoomed l ->
            l

        Zooming bef x aft ->
            bef ++ [ x ] ++ aft


{-| Retrieves the zoomed element, if it exists
-}
zoomed : ZoomList a -> Maybe a
zoomed zlist =
    case zlist of
        UnZoomed _ ->
            Nothing

        Zooming _ x _ ->
            Just x


length : ZoomList a -> Int
length =
    toList >> List.length


{-| Prefixed version of `List.(::)`
-}
prepend : a -> ZoomList a -> ZoomList a
prepend x xs =
    case xs of
        UnZoomed l ->
            UnZoomed (x :: l)

        Zooming bef y aft ->
            Zooming (x :: bef) y aft


{-| Moves the zoom focus to the next element
-}
next : ZoomList a -> Maybe (ZoomList a)
next zlist =
    case zlist of
        UnZoomed [] ->
            Nothing

        UnZoomed (x :: xs) ->
            Just (Zooming [] x xs)

        Zooming bef x [] ->
            Nothing

        Zooming bef x (a :: aft) ->
            Just (Zooming (bef ++ [ x ]) a aft)


{-| Moves the zoom focus to the previous element
-}
prev : ZoomList a -> Maybe (ZoomList a)
prev zlist =
    let
        takeLast : List a -> Maybe ( List a, a )
        takeLast list =
            case list of
                [] ->
                    Nothing

                x :: [] ->
                    Just ( [], x )

                -- [note] this branch must come last
                x :: xs ->
                    takeLast xs
                        |> Maybe.map
                            (Tuple.mapFirst ((::) x))
    in
        case zlist of
            UnZoomed l ->
                takeLast l |> Maybe.map (\( xs, x ) -> Zooming xs x [])

            Zooming bef x aft ->
                takeLast bef
                    |> Maybe.map (\( ys, y ) -> Zooming ys y (x :: aft))


{-| Move the focus directly to the element at an index.
Returns `Nothing` if the index is out of bound
-}
jumpTo : Int -> ZoomList a -> Maybe (ZoomList a)
jumpTo index =
    unzoom
        >> next
        >> flip (List.foldr (always (Maybe.andThen next)))
            (List.repeat index ())


unzoom : ZoomList a -> ZoomList a
unzoom =
    UnZoomed << toList


{-| Updates the zoomed eleemtn.
Returning a `Nothing` removes the zoomed element, and returns an unzoomed list.
-}
updateZoomed : (a -> Maybe a) -> ZoomList a -> ZoomList a
updateZoomed upd zlist =
    case zlist of
        UnZoomed l ->
            UnZoomed l

        Zooming bef x aft ->
            case upd x of
                Nothing ->
                    UnZoomed (bef ++ aft)

                Just newX ->
                    Zooming bef newX aft


{-| Like `List.foldr`, but takes two functions to map:
one for the unzoomed elements, and one for the zoomed element
-}
map : (a -> b) -> (a -> b) -> ZoomList a -> ZoomList b
map unzoomF zoomF zlist =
    let
        mapUnzoomF =
            List.map unzoomF
    in
        case zlist of
            UnZoomed l ->
                UnZoomed (mapUnzoomF l)

            Zooming bef x aft ->
                Zooming (mapUnzoomF bef) (zoomF x) (mapUnzoomF aft)


{-| Like `List.foldr`, but takes two types of accumulator:
one for the unzoomed elements, and one for the zoomed element
-}
foldr : (a -> b -> b) -> (a -> b -> b) -> b -> ZoomList a -> b
foldr unzoomAcc zoomAcc b zlist =
    let
        foldrUnzoom =
            List.foldr unzoomAcc
    in
        case zlist of
            UnZoomed l ->
                foldrUnzoom b l

            Zooming bef x aft ->
                flip foldrUnzoom bef <|
                    zoomAcc x <|
                        foldrUnzoom b aft
