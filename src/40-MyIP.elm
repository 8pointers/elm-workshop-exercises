module Main exposing (..)

import Html exposing (program, div, span, button, text)
import RemoteData exposing (WebData)
import Http


type Msg
    = OnFetchIP (WebData String)


fetchIP : Cmd Msg
fetchIP =
    Http.getString "https://api.ipify.org"
        |> RemoteData.sendRequest
        |> Cmd.map OnFetchIP


update : Msg -> a -> ( Maybe String, Cmd msg )
update msg model =
    case msg of
        OnFetchIP response ->
            ( RemoteData.toMaybe response, Cmd.none )


view : Maybe String -> Html.Html msg
view model =
    div []
        [ span []
            [ text
                (case model of
                    Nothing ->
                        ""

                    Just n ->
                        n
                )
            ]
        ]


main : Program Never (Maybe String) Msg
main =
    program { init = ( Nothing, fetchIP ), view = view, update = update, subscriptions = (\model -> Sub.none) }
