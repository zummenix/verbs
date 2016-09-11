module GameTests exposing (main)

import Test exposing (..)
import Test.Runner.Html
import Expect
import Game
    exposing
        ( Game
        , initGame
        , currentRoundQuestion
        , currentRoundPosition
        , nextRoundQuestion
        , numberOfRemainingRoundQuestions
        , numberOfRoundQuestions
        , numberOfRemainingQuestions
        , maxNumberOfQuestions
        )


main : Program Never
main =
    Test.Runner.Html.run all


all : Test
all =
    describe "Game Logic"
        [ test "max number of questions should be 5" <|
            \() ->
                Expect.equal maxNumberOfQuestions 5
        , test "empty game shouldn't have any questions" <|
            \() ->
                Expect.equal (currentRoundQuestion emptyGame) Nothing
        , test "empty game should have round position 0" <|
            \() ->
                Expect.equal (currentRoundPosition emptyGame) 0
        , test "empty game should have 0 remaining round questions" <|
            \() ->
                Expect.equal (numberOfRemainingRoundQuestions emptyGame) 0
        , test "empty game should have 0 round questions" <|
            \() ->
                Expect.equal (numberOfRoundQuestions emptyGame) 0
        , test "empty game should have 0 remaining questions" <|
            \() ->
                Expect.equal (numberOfRemainingQuestions emptyGame) 0
        , test "game with 5 questions should have correct current question" <|
            \() ->
                Expect.equal (currentRoundQuestion gameWithFiveQuestions) (Just "a")
        , test "game with 5 questions should have round position 0" <|
            \() ->
                Expect.equal (currentRoundPosition gameWithFiveQuestions) 0
        , test "game with 5 questions should have 5 remaining round questions" <|
            \() ->
                Expect.equal (numberOfRemainingRoundQuestions gameWithFiveQuestions) 5
        , test "game with 5 questions should have 5 round questions" <|
            \() ->
                Expect.equal (numberOfRoundQuestions gameWithFiveQuestions) 5
        , test "game with 5 questions should have 5 remaining questions" <|
            \() ->
                Expect.equal (numberOfRemainingQuestions gameWithFiveQuestions) 5
        , test "game with 5 questions (second question) should have correct current question" <|
            \() ->
                Expect.equal (currentRoundQuestion gameWithFiveQuestionsSecondRoundQuestion) (Just "b")
        , test "game with 5 questions (second question) should have round position 1" <|
            \() ->
                Expect.equal (currentRoundPosition gameWithFiveQuestionsSecondRoundQuestion) 1
        , test "game with 5 questions (second question) should have 4 remaining round questions" <|
            \() ->
                Expect.equal (numberOfRemainingRoundQuestions gameWithFiveQuestionsSecondRoundQuestion) 4
        , test "game with 5 questions (second question) should have 5 round questions" <|
            \() ->
                Expect.equal (numberOfRoundQuestions gameWithFiveQuestionsSecondRoundQuestion) 5
        , test "game with 5 questions (second question) should have 4 remaining questions" <|
            \() ->
                Expect.equal (numberOfRemainingQuestions gameWithFiveQuestionsSecondRoundQuestion) 4
        , test "game with 5 questions (third question) should have correct current question" <|
            \() ->
                Expect.equal (currentRoundQuestion gameWithFiveQuestionsThirdRoundQuestion) (Just "c")
        , test "game with 5 questions (third question) should have round position 2" <|
            \() ->
                Expect.equal (currentRoundPosition gameWithFiveQuestionsThirdRoundQuestion) 2
        , test "game with 5 questions (third question) should have 3 remaining round questions" <|
            \() ->
                Expect.equal (numberOfRemainingRoundQuestions gameWithFiveQuestionsThirdRoundQuestion) 3
        , test "game with 5 questions (third question) should have 5 round questions" <|
            \() ->
                Expect.equal (numberOfRoundQuestions gameWithFiveQuestionsThirdRoundQuestion) 5
        , test "game with 5 questions (third question) should have 3 remaining questions" <|
            \() ->
                Expect.equal (numberOfRemainingQuestions gameWithFiveQuestionsThirdRoundQuestion) 3
        , test "game with 7 questions should have 5 remaining round questions" <|
            \() ->
                Expect.equal (numberOfRemainingRoundQuestions gameWithSevenQuestions) 5
        , test "game with 7 questions should have 5 round questions" <|
            \() ->
                Expect.equal (numberOfRoundQuestions gameWithSevenQuestions) 5
        , test "game with 7 questions should have 7 remaining questions" <|
            \() ->
                Expect.equal (numberOfRemainingQuestions gameWithSevenQuestions) 7
        , test "game with 7 questions (skip round) should have correct current question" <|
            \() ->
                Expect.equal (currentRoundQuestion gameWithSevenQuestionsSkipRound) (Just "f")
        , test "game with 7 questions (skip round) should have round position 0" <|
            \() ->
                Expect.equal (currentRoundPosition gameWithSevenQuestionsSkipRound) 0
        , test "game with 7 questions (skip round) should have 2 remaining round questions" <|
            \() ->
                Expect.equal (numberOfRemainingRoundQuestions gameWithSevenQuestionsSkipRound) 2
        , test "game with 7 questions (skip round) should have 2 round questions" <|
            \() ->
                Expect.equal (numberOfRoundQuestions gameWithSevenQuestionsSkipRound) 2
        , test "game with 7 questions (skip round) should have 2 remaining questions" <|
            \() ->
                Expect.equal (numberOfRemainingQuestions gameWithSevenQuestionsSkipRound) 2
        , test "game with 7 questions (finished game) shouldn't have current question" <|
            \() ->
                Expect.equal (currentRoundQuestion gameWithSevenQuestionsFinishedGame) Nothing
        , test "game with 7 questions (finished game) should have round position 0" <|
            \() ->
                Expect.equal (currentRoundPosition gameWithSevenQuestionsFinishedGame) 0
        , test "game with 7 questions (finished game) should have 0 remaining round questions" <|
            \() ->
                Expect.equal (numberOfRemainingRoundQuestions gameWithSevenQuestionsFinishedGame) 0
        , test "game with 7 questions (finished game) should have 0 round questions" <|
            \() ->
                Expect.equal (numberOfRoundQuestions gameWithSevenQuestionsFinishedGame) 0
        , test "game with 7 questions (finished game) should have 0 remaining questions" <|
            \() ->
                Expect.equal (numberOfRemainingQuestions gameWithSevenQuestionsFinishedGame) 0
        ]


emptyGame : Game
emptyGame =
    initGame []


gameWithFiveQuestions : Game
gameWithFiveQuestions =
    initGame [ "a", "b", "c", "d", "e" ]


gameWithFiveQuestionsSecondRoundQuestion : Game
gameWithFiveQuestionsSecondRoundQuestion =
    nextRoundQuestion gameWithFiveQuestions


gameWithFiveQuestionsThirdRoundQuestion : Game
gameWithFiveQuestionsThirdRoundQuestion =
    nextRoundQuestion gameWithFiveQuestionsSecondRoundQuestion


gameWithSevenQuestions : Game
gameWithSevenQuestions =
    initGame [ "a", "b", "c", "d", "e", "f", "g" ]


gameWithSevenQuestionsSkipRound : Game
gameWithSevenQuestionsSkipRound =
    gameWithSevenQuestions
        |> nextRoundQuestion
        |> nextRoundQuestion
        |> nextRoundQuestion
        |> nextRoundQuestion
        |> nextRoundQuestion


gameWithSevenQuestionsFinishedGame : Game
gameWithSevenQuestionsFinishedGame =
    gameWithSevenQuestionsSkipRound |> nextRoundQuestion |> nextRoundQuestion
