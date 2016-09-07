module GameTests exposing (main)

import Test exposing (..)
import Test.Runner.Html
import Expect
import Game exposing (Game, initGame, Round, initRound, nextRound, currentQuestion, maxNumberOfQuestions)


main : Program Never
main =
    Test.Runner.Html.run all


all : Test
all =
    describe "Game Logic"
        [ test "max number of questions should be 7" <|
            \() ->
                Expect.equal maxNumberOfQuestions 7
        , test "empty game shouldn't have any questions" <|
            \() ->
                let
                    ( _, round ) =
                        initGame [] |> nextRound
                in
                    Expect.equal (currentQuestion round) Nothing
        ]

