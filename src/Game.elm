module Game exposing (..)


type alias Game =
    { questions : List String
    , roundQuestions : List String
    , roundPosition : Int
    }


initGame : List String -> Game
initGame questions =
    let
        game =
            { questions = questions
            , roundQuestions = []
            , roundPosition = 0
            }
    in
        nextRoundQuestion game


currentRoundQuestion : Game -> Maybe String
currentRoundQuestion game =
    List.head game.roundQuestions


currentRoundPosition : Game -> Int
currentRoundPosition game =
    game.roundPosition


nextRoundQuestion : Game -> Game
nextRoundQuestion game =
    let
        roundQuestions =
            List.drop 1 game.roundQuestions
    in
        case List.head roundQuestions of
            Just _ ->
                { game | roundQuestions = roundQuestions, roundPosition = game.roundPosition + 1 }

            Nothing ->
                let
                    allQuestions =
                        roundQuestions ++ game.questions

                    roundQuestions =
                        List.take maxNumberOfQuestions allQuestions

                    questions =
                        List.drop maxNumberOfQuestions allQuestions
                in
                    { questions = questions
                    , roundQuestions = roundQuestions
                    , roundPosition = 0
                    }


numberOfRemainingRoundQuestions : Game -> Int
numberOfRemainingRoundQuestions game =
    List.length game.roundQuestions


numberOfRoundQuestions : Game -> Int
numberOfRoundQuestions game =
    List.length game.roundQuestions + game.roundPosition


numberOfRemainingQuestions : Game -> Int
numberOfRemainingQuestions game =
    (List.length game.questions) + (List.length game.roundQuestions)


maxNumberOfQuestions : Int
maxNumberOfQuestions =
    5
