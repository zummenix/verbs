module QuestionTests exposing (all)

import Test exposing (..)
import Expect
import Question


all : Test
all =
    describe "Question Logic"
        [ test "words separation" <|
            \() ->
                Expect.equal (Question.words "a,b,c") [ [ "a" ], [ "b" ], [ "c" ] ]
        , test "words separation with spaces" <|
            \() ->
                Expect.equal (Question.words "b, c , d  ") [ [ "b" ], [ "c" ], [ "d" ] ]
        , test "words separation with spaces and slashes" <|
            \() ->
                Expect.equal (Question.words "b, c/a , d /e /f  ") [ [ "b" ], [ "c", "a" ], [ "d", "e", "f" ] ]
        , test "answers should be correct" <|
            \() ->
                Expect.equal (Question.validate (Question.words "a,b,c") [ "a", "b", "c" ]) True
        , test "answers should not be correct" <|
            \() ->
                Expect.equal (Question.validate (Question.words "a,b,c") [ "b", "b", "c" ]) False
        ]
