module Functions exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Functions-1"
        [ test "Should understand named functions" <|
            \_ ->
                let
                    f n =
                        n + 1
                in
                    Expect.equal 124 (f 123)
        , test "Should understand anonymous functions" <|
            \_ ->
                let
                    f =
                        \n -> n + 1
                in
                    Expect.equal 124 (f 123)
        ]
