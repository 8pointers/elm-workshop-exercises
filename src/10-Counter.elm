module Main exposing (..)

import Html exposing (beginnerProgram, div, span, button, text)
import Html.Events exposing (onClick)


type Msg
    = Increment
    | Decrement


update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : a -> Html.Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , span [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


main : Program Never number Msg
main =
    beginnerProgram { model = 0, view = view, update = update }
