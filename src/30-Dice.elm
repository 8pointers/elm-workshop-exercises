module Main exposing (..)

import Html exposing (program, div, span, button, text)
import Html.Events exposing (onClick)
import Random


type Msg
    = RollDice
    | StopDice Int


update : Msg -> Maybe Int -> ( Maybe Int, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( Nothing, Random.generate StopDice (Random.int 1 6) )

        StopDice n ->
            ( Just n, Cmd.none )


view : Maybe Int -> Html.Html Msg
view model =
    div []
        [ button [ onClick RollDice ] [ text "Roll" ]
        , span []
            [ text
                (case model of
                    Nothing ->
                        ""

                    Just n ->
                        toString n
                )
            ]
        ]


main : Program Never (Maybe Int) Msg
main =
    program { init = ( Nothing, Cmd.none ), view = view, update = update, subscriptions = (\model -> Sub.none) }
