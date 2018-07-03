module Main exposing (..)

import Html exposing (beginnerProgram, div, span, button, text, node)
import Html.Attributes exposing (class, rel, style, href, classList)
import Html.Events exposing (onClick)
import Dict


main : Program Never (Dict.Dict ( Int, Int ) Bool) Msg
main =
    beginnerProgram { model = model, view = view, update = update }


model : Dict.Dict ( Int, Int ) Bool
model =
    Dict.empty


type Msg
    = Tick
    | Toggle ( Int, Int )


deltas : List ( Int, Int, Int )
deltas =
    List.range 0 8
        |> List.map
            (\i ->
                ( i // 3 - 1
                , i % 3 - 1
                , if i == 4 then
                    0
                  else
                    1
                )
            )


neighbours : Dict.Dict ( Int, Int ) Bool -> Dict.Dict ( Int, Int ) Int
neighbours currentState =
    Dict.keys currentState
        |> List.map (\( row, column ) -> (List.map (\( deltaRow, deltaColumn, delta ) -> ( row + deltaRow, column + deltaColumn, delta )) deltas))
        |> List.foldl List.append []
        |> List.foldl
            (\( row, column, delta ) ->
                (Dict.update ( row, column )
                    (\neighbours ->
                        case neighbours of
                            Nothing ->
                                Just delta

                            Just n ->
                                Just (n + delta)
                    )
                )
            )
            Dict.empty


tick : Dict.Dict ( Int, Int ) Bool -> Dict.Dict ( Int, Int ) Bool
tick model =
    let
        n =
            neighbours model
    in
        Dict.toList n
            |> List.filter (\( cell, numNeighbours ) -> Dict.member cell model && numNeighbours == 2 || numNeighbours == 3)
            |> List.map (\( cell, _ ) -> ( cell, True ))
            |> Dict.fromList


toggle : ( Int, Int ) -> Dict.Dict ( Int, Int ) Bool -> Dict.Dict ( Int, Int ) Bool
toggle cell =
    Dict.update cell
        (\isAlive ->
            if isAlive == Just True then
                Nothing
            else
                Just True
        )


update : Msg -> Dict.Dict ( Int, Int ) Bool -> Dict.Dict ( Int, Int ) Bool
update msg =
    case msg of
        Tick ->
            tick

        Toggle cell ->
            toggle cell


view : Dict.Dict ( Int, Int ) Bool -> Html.Html Msg
view model =
    let
        n : number
        n =
            10

        cellSize : number
        cellSize =
            20
    in
        div []
            [ node "link" [ rel "stylesheet", href "style.css" ] []
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
