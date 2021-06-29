module Lev exposing (..)

import Dict
import List exposing (foldl, length, minimum, range)



--  // left is always the longest
--     let prev = Array.from({ length: right.length + 1 }, (_, i) => i);
--     let i = 0;
--     for (const c1 of left) {
--         let current = [i + 1];
--         let j = 0;
--         for (const c2 of right) {
--             const insertions = prev[j + 1] + 1;
--             const deletions = current[j] + 1;
--             const substitutions = prev[j] + (c1 != c2 ? 1 : 0);
--             current.push(Math.min(insertions, deletions, substitutions));
--             j += 1;
--         }
--         prev = current;
--         i += 1;
--     }
--     return prev[prev.length - 1];


last : a -> List a -> a
last x xs =
    case xs of
        [] ->
            x

        [ y ] ->
            y

        _ :: rest ->
            last x rest


scanl : (b -> a -> b) -> b -> List a -> List b
scanl f y lst =
    case lst of
        [] ->
            [ y ]

        x :: xs ->
            y :: scanl f (f y x) xs


zip3 : List a -> List b -> List c -> List ( a, b, c )
zip3 a b c =
    case a of
        [] ->
            []

        x :: xs ->
            case b of
                [] ->
                    []

                y :: ys ->
                    case c of
                        [] ->
                            []

                        z :: zs ->
                            ( x, y, z ) :: zip3 xs ys zs


distance : String -> String -> Int
distance a b =
    let
        s1 =
            String.toList a

        s2 =
            String.toList b

        transform : Char -> List Int -> List Int
        transform c ns =
            let
                calc : Int -> ( Char, Int, Int ) -> Int
                calc z ( c1, x, y ) =
                    Maybe.withDefault 0 <|
                        minimum
                            [ y + 1
                            , z + 1
                            , x
                                + (if c1 == c then
                                    0

                                   else
                                    1
                                  )
                            ]
            in
            case ns of
                [] ->
                    [ 0 ]

                n :: ns1 ->
                    scanl calc (n + 1) <| zip3 s1 ns ns1
    in
    last 0 <| foldl transform (range 0 <| length s1) s2


nsimilar : Int -> String -> List String -> List String
nsimilar n w db =
    let
        cache =
            Dict.fromList <| List.map (\x -> ( x, distance w x )) db
    in
    db |> List.sortBy (\x -> Maybe.withDefault 0 (Dict.get x cache)) |> List.take n
