module Route exposing (..)

import Url exposing (Url)
import Url.Parser as P exposing (Parser)


type Route
    = Main
    | NotFound


parseUrl : Url -> Route
parseUrl url =
    case P.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    P.oneOf
        [ P.map Main P.top
        , P.map Main (P.s "pokemon")
        ]
