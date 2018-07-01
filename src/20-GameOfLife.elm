module Main exposing (..)

import Html exposing (beginnerProgram, div, span, button, text, node)
import Html.Attributes exposing (class, rel, style, href, classList)
import Html.Events exposing (onClick)
import Dict


main : Program Never (Dict.Dict ( Int, Int ) Bool) Msg
main =
    beginnerProgram { model = model, view = view, update = update }


model : Dict.Dict k v
model =
    Dict.empty


type Msg
    = Tick
    | Toggle ( Int, Int )


tick : Dict.Dict ( Int, Int ) Bool -> Dict.Dict ( Int, Int ) Bool
tick model =
    Dict.empty


update : Msg -> Dict.Dict ( Int, Int ) Bool -> Dict.Dict ( Int, Int ) Bool
update msg model =
    case msg of
        Tick ->
            tick model

        Toggle ( row, column ) ->
            Dict.update ( row, column )
                (\isAlive ->
                    case isAlive of
                        Nothing ->
                            Just True

                        Just True ->
                            Nothing

                        Just False ->
                            Just True
                )
                model


n : number
n =
    10


cellSize : number
cellSize =
    20


view : Dict.Dict ( Int, Int ) v -> Html.Html Msg
view model =
    div []
        [ node "link" [ rel "stylesheet", href "style.css" ] []
        , span [] [ text (toString model) ]
        , div [ style [ ( "width", toString (cellSize * n) ++ "px" ), ( "height", toString (cellSize * n) ++ "px" ) ] ]
            (List.range 0 (n * n - 1)
                |> List.map (\i -> ( i // n, i % n ))
                |> List.map (\( row, column ) -> ( row, column, Dict.member ( row, column ) model ))
                |> List.map
                    (\( i, j, isAlive ) ->
                        div
                            [ style
                                [ ( "top", toString (cellSize * i) )
                                , ( "left", toString (cellSize * j) )
                                , ( "width", "20px" )
                                , ( "height", "20px" )
                                ]
                            , classList [ ( "cell", True ), ( "alive", isAlive ) ]
                            , onClick (Toggle ( i, j ))
                            ]
                            []
                    )
            )
        , button [ onClick Tick ] [ text "Tick" ]
        ]
