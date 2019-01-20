module Material exposing
    ( Material
    , Resource(..)
    , allResources
    , create
    , empty
    , fold
    , lookup
    , map
    , map2
    , map3
    , resourceFromString
    , set
    , shorthand
    , toList
    , traverseMaybe
    , trySubtract
    , tryUpdate
    , update
    , values
    )


type Resource
    = Log
    | Food
    | Bandage
    | Bullet


allResources : List Resource
allResources =
    [ Log, Food, Bandage, Bullet ]


resourceFromString : String -> Maybe Resource
resourceFromString str =
    case str of
        "log" ->
            Just Log

        "food" ->
            Just Food

        "bandage" ->
            Just Bandage

        "bullet" ->
            Just Bullet

        _ ->
            Nothing


shorthand : Resource -> String
shorthand =
    String.toLower << String.left 1 << toString


type alias Material a =
    { log : a
    , food : a
    , bandage : a
    , bullet : a
    }


lookup : Resource -> Material a -> a
lookup rsr =
    case rsr of
        Log ->
            .log

        Food ->
            .food

        Bandage ->
            .bandage

        Bullet ->
            .bullet


create : (Resource -> a) -> Material a
create f =
    { log = f Log
    , food = f Food
    , bandage = f Bandage
    , bullet = f Bullet
    }


empty : Material Int
empty =
    create (always 0)


toList : Material a -> List ( Resource, a )
toList mat =
    List.map (\rsr -> ( rsr, lookup rsr mat )) allResources


values : Material a -> List a
values =
    toList >> List.map Tuple.second


map : (Resource -> a -> b) -> Material a -> Material b
map f mat =
    create (\rsr -> f rsr (lookup rsr mat))


map2 :
    (Resource -> a -> b -> c)
    -> Material a
    -> Material b
    -> Material c
map2 f mat =
    map (\rsr -> f rsr (lookup rsr mat))


map3 :
    (Resource -> a -> b -> c -> d)
    -> Material a
    -> Material b
    -> Material c
    -> Material d
map3 f mat =
    map2 (\rsr -> f rsr (lookup rsr mat))


traverseMaybe : Material (Maybe a) -> Maybe (Material a)
traverseMaybe mat =
    List.foldr
        (\rsr ->
            Maybe.andThen
                (\m ->
                    Maybe.map
                        (\a -> update rsr (always a) m)
                        (lookup rsr mat)
                )
        )
        -- [hack] grab from Log
        (Maybe.map
            (\a -> create (always a))
            (lookup Log mat)
        )
        allResources


set : Resource -> a -> Material a -> Material a
set resource =
    update resource << always


update : Resource -> (a -> a) -> Material a -> Material a
update resource upd =
    map
        (\rsr ->
            if rsr == resource then
                upd

            else
                identity
        )


tryUpdate :
    Resource
    -> (a -> Maybe a)
    -> Material a
    -> Maybe (Material a)
tryUpdate resource f =
    traverseMaybe
        << map
            (\rsr ->
                if rsr == resource then
                    f

                else
                    Just
            )


fold : (Resource -> a -> b -> b) -> b -> Material a -> b
fold acc b =
    List.foldr (uncurry acc) b << toList


trySubtract : Material number -> Material number -> Maybe (Material number)
trySubtract =
    curry <|
        traverseMaybe
            << uncurry
                (map2
                    (always
                        (\a b ->
                            let
                                d =
                                    b - a
                            in
                            if d >= 0 then
                                Just d

                            else
                                Nothing
                        )
                    )
                )
