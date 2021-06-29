-- Generated by https://quicktype.io


module Pokemon exposing (Pokemon, Stats, Type, pokemonDecoder)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as D


type alias Pokemon =
    { abilities : Array Ability
    , id : Int
    , moves : Array Move
    , name : String
    , sprite : Sprite
    , stats : Stats
    , types : Types
    , weight : Int
    }


type alias Stats =
    Dict String Int


type alias Types =
    { primary : Type, secondary : Maybe Type }


type alias Ability =
    { name : String
    , url : String
    , isHidden : Bool
    , slot : Int
    }


type alias Type =
    { slot : Int
    , name : String
    , url : String
    }


type alias Move =
    { name : String
    , url : String
    }


type alias Sprite =
    { frontDefault : String
    , backDefault : Maybe String
    }


pokemonDecoder : D.Decoder Pokemon
pokemonDecoder =
    D.map8 Pokemon
        (D.field "abilities" (D.array abilityDecoder))
        (D.field "id" D.int)
        (D.field "moves" (D.array moveDecoder))
        (D.field "name" D.string)
        (D.field "sprites" spriteDecoder)
        (D.field "stats" statDecoder)
        (D.field "types" typesDecoder)
        (D.field "weight" D.int)


abilityDecoder : D.Decoder Ability
abilityDecoder =
    D.map4 Ability
        (D.field "ability" <| D.field "name" D.string)
        (D.field "ability" <| D.field "url" D.string)
        (D.field "is_hidden" D.bool)
        (D.field "slot" D.int)


moveDecoder : D.Decoder Move
moveDecoder =
    D.map2 Move
        (D.field "move" <| D.field "name" D.string)
        (D.field "move" <| D.field "url" D.string)


spriteDecoder : D.Decoder Sprite
spriteDecoder =
    D.map2 Sprite
        (D.field "front_default" D.string)
        (D.field "back_default" (D.maybe D.string))


type alias Stat =
    { base : Int, name : String }


statDecoder : D.Decoder (Dict String Int)
statDecoder =
    let
        statDec =
            D.map2 Stat
                (D.field "base_stat" D.int)
                (D.field "stat" (D.field "name" D.string))

        s arr =
            Array.foldl ff Dict.empty arr

        ff : Stat -> Dict String Int -> Dict String Int
        ff { base, name } d =
            Dict.insert name base d
    in
    D.map s (D.array statDec)


typeDecoder : D.Decoder Type
typeDecoder =
    D.map3 Type
        (D.field "slot" D.int)
        (D.field "type" (D.field "name" D.string))
        (D.field "type" (D.field "url" D.string))


typesDecoder : D.Decoder Types
typesDecoder =
    let
        transform : Array Type -> Types
        transform arr =
            let
                lst =
                    List.sortBy .slot <| Array.toList arr
            in
            case lst of
                [ f, s ] ->
                    Types f (Just s)

                [ f ] ->
                    Types f Nothing

                _ ->
                    -- unreachable
                    Types (Type 0 "" "") Nothing
    in
    D.map transform (D.array typeDecoder)
