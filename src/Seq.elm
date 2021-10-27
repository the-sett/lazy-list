module Seq exposing
    ( Seq(..)
    , cons, empty, singleton
    , isEmpty, head, tail, headAndTail, member, length
    , toList, fromList, toArray, fromArray
    , map, zip, reduce, reductions, flatten, append, foldl, foldr
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

@docs map, zip, reduce, reductions, flatten, append, foldl, foldr


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


{-| The lazy list type.
-}
type Seq a
    = Nil
    | Cons a (() -> Seq a)


{-| Add a value to the front of a list.
-}
cons : a -> Seq a -> Seq a
cons a list =
    Cons a (\() -> list)


{-| Create an empty list.
-}
empty : Seq a
empty =
    Nil


{-| Create a singleton list.
-}
singleton : a -> Seq a
singleton a =
    cons a empty


{-| Detect if a list is empty or not.
-}
isEmpty : Seq a -> Bool
isEmpty list =
    case list of
        Nil ->
            True

        _ ->
            False


{-| Get the head of a list.
-}
head : Seq a -> Maybe a
head list =
    case list of
        Nil ->
            Nothing

        Cons first _ ->
            Just first


{-| Get the tail of a list.
-}
tail : Seq a -> Maybe (Seq a)
tail list =
    case list of
        Nil ->
            Nothing

        Cons _ rest ->
            Just <| rest ()


{-| Get the head and tail of a list.
-}
headAndTail : Seq a -> Maybe ( a, Seq a )
headAndTail list =
    case list of
        Nil ->
            Nothing

        Cons first rest ->
            Just ( first, rest () )


{-| Test if a value is a member of a list.
-}
member : a -> Seq a -> Bool
member a list =
    case list of
        Nil ->
            False

        Cons first rest ->
            first == a || member a (rest ())


{-| Get the length of a lazy list - provided it is finite.
-}
length : Seq a -> Int
length =
    reduce (\_ n -> n + 1) 0


{-| Convert a lazy list to a normal list.
-}
toList : Seq a -> List a
toList list =
    let
        helper : Seq a -> List a -> List a
        helper coll acc =
            case coll of
                Nil ->
                    List.reverse acc

                Cons first rest ->
                    helper (rest ()) (first :: acc)
    in
    helper list []


{-| Convert a normal list to a lazy list.
-}
fromList : List a -> Seq a
fromList =
    List.foldr cons empty


{-| Convert a lazy list to an array.
-}
toArray : Seq a -> Array a
toArray list =
    toList list |> Array.fromList


{-| Convert an array to a lazy list.
-}
fromArray : Array a -> Seq a
fromArray =
    Array.foldr cons empty


{-| Map a function over a list.
-}
map : (a -> b) -> Seq a -> Seq b
map f list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            Cons (f first) (\() -> map f (rest ()))


{-| Zip two lists together.
-}
zip : Seq a -> Seq b -> Seq ( a, b )
zip =
    map2 Tuple.pair


{-| Zip three lists together.
-}
zip2 : Seq a -> Seq b -> Seq c -> Seq ( a, b, c )
zip2 =
    let
        triple a b c =
            ( a, b, c )
    in
    map3 triple


{-| Reduce a list with a given reducer and an initial value.

Example :
reduce (+) 0 (fromList [1, 2, 3, 4]) == 10

-}
reduce : (a -> b -> b) -> b -> Seq a -> b
reduce reducer b list =
    case list of
        Nil ->
            b

        Cons first rest ->
            reduce reducer (reducer first b) (rest ())


{-| Produce intermediate values of reduce.
-}
reductions : (a -> b -> b) -> b -> Seq a -> Seq b
reductions reducer b list =
    case list of
        Nil ->
            singleton b

        Cons first rest ->
            Cons b (\_ -> reductions reducer (reducer first b) (rest ()))


{-| Flatten a list of lists into a single list by appending all the inner
lists into one big list.
-}
flatten : Seq (Seq a) -> Seq a
flatten list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            append first (flatten (rest ()))


{-| Append a list to another list.
-}
append : Seq a -> Seq a -> Seq a
append list1 list2 =
    case list1 of
        Nil ->
            list2

        Cons first rest ->
            Cons first (\() -> append (rest ()) list2)


{-| Analogous to `List.foldl`. Is an alias for `reduce`.
-}
foldl : (a -> b -> b) -> b -> Seq a -> b
foldl =
    reduce


{-| Analogous to `List.foldr`.
-}
foldr : (a -> b -> b) -> b -> Seq a -> b
foldr reducer b list =
    Array.foldr reducer b (toArray list)


{-| Places the given value between all members of the given list.
-}
intersperse : a -> Seq a -> Seq a
intersperse a list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            case rest () of
                Nil ->
                    cons first empty

                rest_ ->
                    cons first <| Cons a (\_ -> intersperse a rest_)


{-| Interleave the elements of a list in another list. The two lists get
interleaved at the end.
-}
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


{-| Reverse a list.
-}
reverse : Seq a -> Seq a
reverse =
    reduce cons empty


{-| Take a list and repeat it ad infinitum.
-}
cycle : Seq a -> Seq a
cycle list =
    case list of
        Cons first rest ->
            Cons first (\() -> append (rest ()) (cycle list))

        Nil ->
            Nil


{-| Create an infinite list of applications of a function on some value.

Equivalent to:

    cons x (cons (f x) (cons (f (f x)) (cons (f (f (f x))) ... -- etc...

-}
iterate : (a -> a) -> a -> Seq a
iterate f a =
    Cons a (\() -> iterate f (f a))


{-| Repeat a value ad infinitum.
Be careful when you use this. The result of this is a truly infinite list.
Do not try calling `reduce` or `toList` on an infinite list as it'll never
finish computing. Make sure you then filter it down to a finite list with `head`
or `take` or something.
-}
repeat : a -> Seq a
repeat a =
    Cons a (\() -> repeat a)


{-| Take at most `n` many values from a list.
-}
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


{-| Take elements from a list as long as the predicate is satisfied.
-}
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


{-| Drop at most `n` many values from a list.
-}
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


{-| Drop elements from a list as long as the predicate is satisfied.
-}
dropWhile : (a -> Bool) -> Seq a -> Seq a
dropWhile predicate list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            if predicate first then
                dropWhile predicate (rest ())

            else
                list


{-| Keep all elements in a list that satisfy the given predicate.
-}
keepIf : (a -> Bool) -> Seq a -> Seq a
keepIf predicate list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            if predicate first then
                Cons first (\() -> keepIf predicate <| rest ())

            else
                keepIf predicate (rest ())


{-| Drop all elements in a list that satisfy the given predicate.
-}
dropIf : (a -> Bool) -> Seq a -> Seq a
dropIf predicate =
    keepIf (\n -> not (predicate n))


{-| Map a function that may fail over a lazy list, keeping only
the values that were successfully transformed.
-}
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
                    filterMap transform (rest ())


{-| Remove all duplicates from a list and return a list of distinct elements.
-}
unique : Seq a -> Seq a
unique list =
    case list of
        Nil ->
            Nil

        Cons first rest ->
            if member first <| rest () then
                unique (rest ())

            else
                Cons first (\() -> unique <| rest ())


{-| Known as `mapN` in some circles. Allows you to apply `map` in cases
where then number of arguments are greater than 5.

The argument order is such that it works well with `|>` chains.

-}
andMap : Seq a -> Seq (a -> b) -> Seq b
andMap listVal listFuncs =
    map2 (<|) listFuncs listVal


{-| Chain list producing operations. Map then flatten.
-}
andThen : (a -> Seq b) -> Seq a -> Seq b
andThen f list =
    map f list |> flatten


{-| The infinite list of counting numbers.

i.e.:

    cons 1 (cons 2 (cons 3 (cons 4 (cons 5 ... -- etc...

-}
numbers : Seq number
numbers =
    iterate ((+) 1) 1


{-| Get the sum of a list of numbers.
-}
sum : Seq number -> number
sum =
    reduce (+) 0


{-| Get the product of a list of numbers.
-}
product : Seq number -> number
product =
    reduce (*) 1


{-| Map a function over 2 lists.
-}
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


{-| Map a function over 3 lists.
-}
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


{-| Create a lazy list containing all possible pairs in the given lazy lists.
-}
product2 : Seq a -> Seq b -> Seq ( a, b )
product2 list1 list2 =
    case seq1 of
        Nil ->
            Nil

        Cons first1 rest1 ->
            case seq2 of
                Nil ->
                    Nil

                Cons first2 rest2 ->
                    let
                        _ =
                            Debug.log "consing" ( first1, first2 )
                    in
                    Cons ( first1, first2 )
                        (\() ->
                            append
                                (product2 (singleton first1) (rest2 ()))
                                (product2 (rest1 ()) (rest2 ()))
                        )



{-| Create a lazy list containing all possible triples in the given lazy lists.
-}
product3 : Seq a -> Seq b -> Seq c -> Seq ( a, b, c )
product3 list1 list2 list3 =
    product2 list1 (product2 list2 list3) |> map (\( a, ( b, c ) ) -> ( a, b, c ))
