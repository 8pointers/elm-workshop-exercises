module Main exposing (..)

import Html exposing (beginnerProgram, div, span, button, text)
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


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , span [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
