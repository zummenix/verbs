module Game exposing (..)


type alias Game =
    { verbs : List String
    }


initGame : List String -> Game
initGame verbs =
    { verbs = verbs }


nextRound : Game -> ( Game, Round )
nextRound game =
    let
        verbs =
            List.take maxNumberOfQuestions game.verbs
    in
        ( initGame <| List.drop maxNumberOfQuestions game.verbs, initRound verbs )


type alias Round =
    { verbs : List String
    , position : Int
    }


initRound : List String -> Round
initRound verbs =
    { verbs = verbs, position = 0 }


currentQuestion : Round -> Maybe String
currentQuestion round =
    List.head round.verbs


currentPosition : Round -> Int
currentPosition round =
    round.position + 1


numberOfQuestions : Round -> Int
numberOfQuestions round =
    List.length round.verbs + round.position


numberOfRemainingQuestions : Round -> Game -> Int
numberOfRemainingQuestions round game =
    (List.length round.verbs) + (List.length game.verbs)


maxNumberOfQuestions : Int
maxNumberOfQuestions =
    7
