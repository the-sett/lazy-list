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
    , zip3, zip4, zip5
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


# All the zips.

@docs zip3, zip4, zip5


# All the Cartesian products.

@docs product2, product3, product4, product5

-}


type Seq a
    = Nil
    | Cons a (() -> Seq a)


cons =
    Debug.todo "Not implemented yet"


empty =
    Debug.todo "Not implemented yet"


singleton =
    Debug.todo "Not implemented yet"


isEmpty =
    Debug.todo "Not implemented yet"


head =
    Debug.todo "Not implemented yet"


tail =
    Debug.todo "Not implemented yet"


headAndTail =
    Debug.todo "Not implemented yet"


member =
    Debug.todo "Not implemented yet"


length =
    Debug.todo "Not implemented yet"


toList =
    Debug.todo "Not implemented yet"


fromList =
    Debug.todo "Not implemented yet"


toArray =
    Debug.todo "Not implemented yet"


fromArray =
    Debug.todo "Not implemented yet"


map =
    Debug.todo "Not implemented yet"


zip =
    Debug.todo "Not implemented yet"


reduce =
    Debug.todo "Not implemented yet"


flatten =
    Debug.todo "Not implemented yet"


append =
    Debug.todo "Not implemented yet"


foldl =
    Debug.todo "Not implemented yet"


foldr =
    Debug.todo "Not implemented yet"


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


zip3 =
    Debug.todo "Not implemented yet"


zip4 =
    Debug.todo "Not implemented yet"


zip5 =
    Debug.todo "Not implemented yet"


product2 =
    Debug.todo "Not implemented yet"


product3 =
    Debug.todo "Not implemented yet"


product4 =
    Debug.todo "Not implemented yet"


product5 =
    Debug.todo "Not implemented yet"
