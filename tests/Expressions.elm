module Expressions exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Expressions-1"
        [ test "Should understand + operator" <|
            \_ ->
                Expect.equal 3 (1 + 2)
        ]
