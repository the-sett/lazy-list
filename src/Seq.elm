module Seq exposing
    ( Seq(..)
    , cons, empty, singleton
    , isEmpty, head, tail, headAndTail, member, length
    , toList, fromList, toArray, fromArray
    , map, zip, reduce, flatten, append, foldl, foldr
    , intersperse, interleave, reverse, cycle, iterate, repeat, take, takeWhile, drop, dropWhile
    , keepIf, dropIf, filterMap, unique
    , andMap, andThen
    , numbers, sum, product
    , map2, map3
    , product2, product3
    )

{-| Lazy list implementation in Elm.


# Types.

@docs Seq


# Constructors.

@docs cons, empty, singleton


# Query operations.

@docs isEmpty, head, tail, headAndTail, member, length


# Conversions.

@docs toList, fromList, toArray, fromArray


# Map, reduce and related operations.

@docs map, zip, reduce, flatten, append, foldl, foldr


# Common operations.

@docs intersperse, interleave, reverse, cycle, iterate, repeat, take, takeWhile, drop, dropWhile


# Filtering operations.

@docs keepIf, dropIf, filterMap, unique


# Chaining operations.

@docs andMap, andThen


# Useful math stuff.

@docs numbers, sum, product


# All the maps.

@docs map2, map3


# All the Cartesian products.

@docs product2, product3

-}

import Array exposing (Array)


type Seq a
    = Nil
    | Cons a (() -> Seq a)


cons : a -> Seq a -> Seq a
cons a list =
    Cons a (\() -> list)


empty : Seq a
empty =
    Nil


singleton : a -> Seq a
singleton a =
    cons a empty


isEmpty : Seq a -> Bool
isEmpty list =
    case list of
        Nil ->
            True

        _ ->
            False


head : Seq a -> Maybe a
head list =
    case list of
        Nil ->
            Nothing

        Cons first _ ->
            Just first


tail : Seq a -> Maybe (Seq a)
tail list =
    case list of
        Nil ->
            Nothing

        Cons _ rest ->
            Just <| rest ()


headAndTail : Seq a -> Maybe ( a, Seq a )
headAndTail list =
    case list of
        Nil ->
            Nothing

        Cons first rest ->
            Just ( first, rest () )


member : a -> Seq a -> Bool
member a list =
    case list of
        Nil ->
            False

        Cons first rest ->
            first == a || member a (rest ())


length : Seq a -> Int
length =
    reduce (\_ n -> n + 1) 0


toList : Seq a -> List a
toList list =
    case list of
        Nil ->
            []

        Cons first rest ->
            first :: toList (rest ())


fromList : List a -> Seq a
fromList =
    List.foldr cons empty


toArray : Seq a -> Array a
toArray list =
    case list of
        Nil ->
            Array.empty

        Cons first rest ->
            Array.append (Array.push first Array.empty) (toArray <| rest ())


fromArray : Array a -> Seq a
fromArray =
    Array.foldr cons empty


map : (a -> b) -> Seq a -> Seq b
map f list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            Cons (f first) (\() -> map f (rest ()))


zip : Seq a -> Seq b -> Seq ( a, b )
zip =
    map2 Tuple.pair


zip2 : Seq a -> Seq b -> Seq c -> Seq ( a, b, c )
zip2 =
    let
        triple a b c =
            ( a, b, c )
    in
    map3 triple


reduce : (a -> b -> b) -> b -> Seq a -> b
reduce reducer b list =
    case list of
        Nil ->
            b

        Cons first rest ->
            reduce reducer (reducer first b) (rest ())


flatten : Seq (Seq a) -> Seq a
flatten list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            append first (flatten (rest ()))


append : Seq a -> Seq a -> Seq a
append list1 list2 =
    case list1 of
        Nil ->
            list2

        Cons first rest ->
            Cons first (\() -> append (rest ()) list2)


foldl : (a -> b -> b) -> b -> Seq a -> b
foldl =
    reduce


foldr : (a -> b -> b) -> b -> Seq a -> b
foldr reducer b list =
    Array.foldr reducer b (toArray list)


intersperse : a -> Seq a -> Seq a
intersperse a list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            case rest () of
                Nil ->
                    cons first empty

                Cons second rest2 ->
                    case rest2 () of
                        Nil ->
                            cons first (cons a (cons second empty))

                        _ ->
                            cons first (cons a (cons second (cons a (intersperse a (rest2 ())))))


interleave : Seq a -> Seq a -> Seq a
interleave list1 list2 =
    case list1 of
        Nil ->
            list2

        Cons first1 rest1 ->
            case list2 of
                Nil ->
                    list1

                Cons first2 rest2 ->
                    cons first1 (cons first2 (interleave (rest1 ()) (rest2 ())))


reverse : Seq a -> Seq a
reverse =
    reduce cons empty


cycle : Seq a -> Seq a
cycle list =
    append list (cycle list)


iterate : (a -> a) -> a -> Seq a
iterate f a =
    Cons a (\() -> iterate f (f a))


repeat : a -> Seq a
repeat a =
    Cons a (\() -> repeat a)


take : Int -> Seq a -> Seq a
take n list =
    if n <= 0 then
        Nil

    else
        case list of
            Nil ->
                Nil

            Cons first rest ->
                Cons first (\() -> take (n - 1) (rest ()))


takeWhile : (a -> Bool) -> Seq a -> Seq a
takeWhile predicate list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            if predicate first then
                Cons first (\() -> takeWhile predicate (rest ()))

            else
                Nil


drop : Int -> Seq a -> Seq a
drop n list =
    if n <= 0 then
        list

    else
        case list of
            Nil ->
                Nil

            Cons first rest ->
                drop (n - 1) (rest ())


dropWhile : (a -> Bool) -> Seq a -> Seq a
dropWhile predicate list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            if predicate first then
                dropWhile predicate <| rest ()

            else
                list


keepIf : (a -> Bool) -> Seq a -> Seq a
keepIf predicate list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            if predicate first then
                Cons first (\() -> keepIf predicate <| rest ())

            else
                keepIf predicate <| rest ()


dropIf : (a -> Bool) -> Seq a -> Seq a
dropIf predicate =
    keepIf (\n -> not (predicate n))


filterMap : (a -> Maybe b) -> Seq a -> Seq b
filterMap transform list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            case transform first of
                Just val ->
                    Cons val (\() -> filterMap transform <| rest ())

                Nothing ->
                    filterMap transform <| rest ()


unique : Seq a -> Seq a
unique list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            if member first <| rest () then
                unique <| rest ()

            else
                Cons first (\() -> unique <| rest ())


andMap : Seq a -> Seq (a -> b) -> Seq b
andMap listVal listFuncs =
    map2 (<|) listFuncs listVal


andThen : (a -> Seq b) -> Seq a -> Seq b
andThen f list =
    map f list |> flatten


numbers : Seq number
numbers =
    iterate ((+) 1) 1


sum : Seq number -> number
sum =
    reduce (+) 0


product : Seq number -> number
product =
    reduce (*) 1


map2 : (a -> b -> c) -> Seq a -> Seq b -> Seq c
map2 f list1 list2 =
    case list1 of
        Nil ->
            Nil

        Cons first1 rest1 ->
            case list2 of
                Nil ->
                    Nil

                Cons first2 rest2 ->
                    Cons (f first1 first2) (\() -> map2 f (rest1 ()) (rest2 ()))


map3 : (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
map3 f list1 list2 list3 =
    case list1 of
        Nil ->
            Nil

        Cons first1 rest1 ->
            case list2 of
                Nil ->
                    Nil

                Cons first2 rest2 ->
                    case list3 of
                        Nil ->
                            Nil

                        Cons first3 rest3 ->
                            Cons (f first1 first2 first3) (\() -> map3 f (rest1 ()) (rest2 ()) (rest3 ()))


product2 : Seq a -> Seq b -> Seq ( a, b )
product2 list1 list2 =
    case list1 of
        Nil ->
            Nil

        Cons first1 rest1 ->
            case list2 of
                Nil ->
                    Nil

                Cons _ _ ->
                    append (map (Tuple.pair first1) list2) (product2 (rest1 ()) list2)


product3 : Seq a -> Seq b -> Seq c -> Seq ( a, b, c )
product3 list1 list2 list3 =
    case list1 of
        Nil ->
            Nil

        Cons first1 rest1 ->
            append (map (\( b, c ) -> ( first1, b, c )) (product2 list2 list3)) (product3 (rest1 ()) list2 list3)
