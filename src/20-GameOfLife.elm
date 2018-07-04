module Main exposing (..)

import Html exposing (beginnerProgram, div, span, button, text, node)
import Html.Attributes exposing (class, rel, style, href, classList)
import Html.Events exposing (onClick)
import Dict


main : Program Never GameState Msg
main =
    beginnerProgram { model = model, view = view, update = update }


type alias Cell =
    ( Int, Int )


type alias GameState =
    Dict.Dict Cell Bool


model : GameState
model =
    Dict.empty


type Msg
    = Tick
    | Toggle Cell


toggle : Cell -> GameState -> GameState
toggle cell =
    Dict.update cell
        (\isAlive ->
            if isAlive == Just True then
                Nothing
            else
                Just True
        )


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


neighbours : GameState -> Dict.Dict Cell Int
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


tick : GameState -> GameState
tick model =
    let
        n =
            neighbours model
    in
        Dict.toList n
            |> List.filter (\( cell, numNeighbours ) -> Dict.member cell model && numNeighbours == 2 || numNeighbours == 3)
            |> List.map (\( cell, _ ) -> ( cell, True ))
            |> Dict.fromList


update : Msg -> GameState -> GameState
update msg =
    case msg of
        Toggle cell ->
            toggle cell

        Tick ->
            tick


view : GameState -> Html.Html Msg
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
