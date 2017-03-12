module Tests exposing (..)

import GameTests
import QuestionTests
import Test exposing (..)


all : Test
all =
    Test.concat
        [ GameTests.all
        , QuestionTests.all
        ]
