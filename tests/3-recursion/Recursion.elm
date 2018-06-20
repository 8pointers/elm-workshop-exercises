module Recursion exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Recursion-1"
        [ test "Should understand named functions" <|
            \_ ->
                let
                    factorial n =
                        if n == 0 then
                            1
                        else
                            n * factorial (n - 1)
                in
                    Expect.equal 120 (factorial 5)
        ]
