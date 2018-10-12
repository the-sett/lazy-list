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
    , map2, map3, map4, map5
    , product2, product3, product4, product5
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

@docs map2, map3, map4, map5


# All the Cartesian products.

@docs product2, product3, product4, product5

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


intersperse =
    Debug.todo "Not implemented yet"


interleave =
    Debug.todo "Not implemented yet"


reverse =
    Debug.todo "Not implemented yet"


cycle =
    Debug.todo "Not implemented yet"


iterate =
    Debug.todo "Not implemented yet"


repeat =
    Debug.todo "Not implemented yet"


take =
    Debug.todo "Not implemented yet"


takeWhile =
    Debug.todo "Not implemented yet"


drop =
    Debug.todo "Not implemented yet"


dropWhile =
    Debug.todo "Not implemented yet"


keepIf =
    Debug.todo "Not implemented yet"


dropIf =
    Debug.todo "Not implemented yet"


filterMap =
    Debug.todo "Not implemented yet"


unique =
    Debug.todo "Not implemented yet"


andMap =
    Debug.todo "Not implemented yet"


andThen =
    Debug.todo "Not implemented yet"


numbers =
    Debug.todo "Not implemented yet"


sum =
    Debug.todo "Not implemented yet"


product =
    Debug.todo "Not implemented yet"


map2 =
    Debug.todo "Not implemented yet"


map3 =
    Debug.todo "Not implemented yet"


map4 =
    Debug.todo "Not implemented yet"


map5 =
    Debug.todo "Not implemented yet"


product2 =
    Debug.todo "Not implemented yet"


product3 =
    Debug.todo "Not implemented yet"


product4 =
    Debug.todo "Not implemented yet"


product5 =
    Debug.todo "Not implemented yet"
