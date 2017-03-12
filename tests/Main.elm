port module Main exposing (..)

import GameTests
import QuestionTests
import Test
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    let
        allTests =
            Test.concat
                [ GameTests.all
                , QuestionTests.all
                ]
    in
        run emit allTests


port emit : ( String, Value ) -> Cmd msg
