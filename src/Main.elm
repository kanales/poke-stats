module Main exposing (..)

import Browser
import Debug exposing (toString)
import Dict exposing (update)
import Html exposing (Html, button, div, form, h1, h2, header, img, input, label, li, node, p, text, ul)
import Html.Attributes exposing (action, class, for, href, id, placeholder, rel, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput, onSubmit, preventDefaultOn)
import Http
import Json.Decode exposing (errorToString)
import Pokemon exposing (Pokemon, Stats, pokemonDecoder)
import Route exposing (Route)


main =
    Browser.element
        { init = init, update = update, subscriptions = subscriptions, view = view }


type Status
    = Failure String
    | Loading
    | Awaiting
    | Success Pokemon


type alias Model =
    { status : Status
    , route : Route
    , data :
        { input : String
        }
    }


type Msg
    = GotPokemon (Result Http.Error Pokemon)
    | PokemonName String
    | RequestPokemon


base : String
base =
    "https://pokeapi.co/api/v2/"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Awaiting, data = { input = "" }, route = Route.Main }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        PokemonName s ->
            ( { m | data = { input = s } }, Cmd.none )

        RequestPokemon ->
            ( { m | status = Loading }
            , Http.get
                { url =
                    let
                        name =
                            String.toLower m.data.input

                        url =
                            base ++ "pokemon/" ++ name
                    in
                    url
                , expect = Http.expectJson GotPokemon pokemonDecoder
                }
            )

        GotPokemon res ->
            case res of
                Ok pkm ->
                    ( { m | status = Success pkm }, Cmd.none )

                Err e ->
                    ( { m | status = Failure (toString e) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        cont =
            case model.status of
                Failure e ->
                    div [ class "alert alert-danger" ]
                        [ p [] [ text "Unable to load pokemon!" ]
                        ]

                Loading ->
                    div [ class "alert alert-info" ] [ text "Loading..." ]

                Success pkm ->
                    viewPokemon pkm

                Awaiting ->
                    div [] []
    in
    div []
        [ node "link" [ rel "stylesheet", href "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" ] []
        , header [ class "px-4 py-5 my-5 text-center" ]
            [ img [ src "https://upload.wikimedia.org/wikipedia/commons/2/23/Pok%C3%A9_Ball.svg", width 72 ] []
            , h1 [ class "display-5 fw-bold" ] [ text "PokeStats" ]
            , p [ class "lead mb-4" ] [ text "A simple page to look up pokemon stats" ]
            ]
        , div [ class "container" ] [ viewInput model ]
        , div [ class "container" ] [ cont ]
        ]


viewInput : Model -> Html Msg
viewInput m =
    form [ onSubmit RequestPokemon, class "my-3" ]
        [ div [ class "mb-3" ]
            [ label [ for "name-input", class "form-label" ] [ text "Name" ]
            , input [ class "form-control", id "name-input", type_ "text", placeholder "ditto", value m.data.input, onInput PokemonName ] []
            ]
        , button [ type_ "submit", onClick RequestPokemon, class "btn btn-primary" ] [ text "submit" ]
        ]


capitalize : String -> String
capitalize =
    let
        handle s =
            String.toUpper (String.left 1 s) ++ String.toLower (String.dropLeft 1 s)
    in
    String.split " "
        >> List.map handle
        >> String.concat


viewPokemon : Pokemon -> Html Msg
viewPokemon pkm =
    div []
        [ div [ class "card my-3" ]
            [ div [ class "card-body" ]
                [ img [ src pkm.sprite.frontDefault ] []
                , h1 [ class "card-title" ] [ text <| capitalize pkm.name ]
                ]
            ]
        , viewStats pkm.stats
        ]


viewStats : Stats -> Html Msg
viewStats stats =
    let
        ratio v =
            100 * (toFloat v / 200)

        width v =
            toString (ratio v) ++ "%"

        color v =
            String.concat
                [ "hsl("
                , String.join ","
                    [ toString (toFloat v / 220 * 200)
                    , "100%"
                    , "50%"
                    ]
                , ")"
                ]

        viewStat : ( String, Int ) -> Html Msg
        viewStat ( k, v ) =
            div [ class "d-flex" ]
                [ div [ class "col" ] [ text k ]
                , div [ class "col-1" ] [ text <| toString v ]
                , div [ class "col" ]
                    [ div
                        [ style "background" (color v)
                        , style "height" "1em"
                        , style "width" (width v)
                        ]
                        []
                    ]
                ]
    in
    div [ class "card my-3" ]
        [ div [ class "card-body" ] [ h2 [ class "card-title" ] [ text "Stats" ] ]
        , Dict.toList stats
            |> List.map viewStat
            |> List.map (\x -> li [ class "list-group-item" ] [ x ])
            |> ul [ class "list-group list-group-flush" ]
        ]
