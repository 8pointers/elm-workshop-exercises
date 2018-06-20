module Main exposing (..)

import Html exposing (beginnerProgram, div, span, button, text, node)
import Html.Attributes exposing (rel, href)
import Html.Events exposing (onClick)


main =
    beginnerProgram { model = model, view = view, update = update }


model =
    0


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


n =
    10


view model =
    div []
        (List.range 0 (n * n - 1)
            |> List.map (\i -> ( i // n, i % n ))
            |> List.map (\( i, j ) -> div [] [ text (toString i ++ "-" ++ toString j) ])
        )
