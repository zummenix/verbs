module Game exposing
    ( Game
    , addToRepeat
    , currentRoundPosition
    , currentRoundQuestion
    , initGame
    , maxNumberOfQuestions
    , nextRoundQuestion
    , numberOfRemainingQuestions
    , numberOfRemainingRoundQuestions
    , numberOfRoundQuestions
    )


type Game
    = Game
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
    nextRoundQuestion (Game game)


currentRoundQuestion : Game -> Maybe String
currentRoundQuestion (Game { roundQuestions }) =
    List.head roundQuestions


currentRoundPosition : Game -> Int
currentRoundPosition (Game { roundPosition }) =
    roundPosition


nextRoundQuestion : Game -> Game
nextRoundQuestion (Game { questions, roundQuestions, roundPosition }) =
    let
        roundQuestionsRemaining =
            List.drop 1 roundQuestions
    in
    case List.head roundQuestionsRemaining of
        Just _ ->
            Game
                { questions = questions
                , roundQuestions = roundQuestionsRemaining
                , roundPosition = roundPosition + 1
                }

        Nothing ->
            let
                allQuestions =
                    roundQuestionsRemaining ++ questions

                roundQuestionsNew =
                    List.take maxNumberOfQuestions allQuestions

                newQuestions =
                    List.drop maxNumberOfQuestions allQuestions
            in
            Game
                { questions = newQuestions
                , roundQuestions = roundQuestionsNew
                , roundPosition = 0
                }


addToRepeat : String -> Game -> Game
addToRepeat question (Game { questions, roundQuestions, roundPosition }) =
    Game
        { questions = question :: questions
        , roundQuestions = roundQuestions
        , roundPosition = roundPosition + 1
        }


numberOfRemainingRoundQuestions : Game -> Int
numberOfRemainingRoundQuestions (Game { roundQuestions }) =
    List.length roundQuestions


numberOfRoundQuestions : Game -> Int
numberOfRoundQuestions (Game { roundQuestions, roundPosition }) =
    List.length roundQuestions + roundPosition


numberOfRemainingQuestions : Game -> Int
numberOfRemainingQuestions (Game { questions, roundQuestions, roundPosition }) =
    List.length questions + List.length roundQuestions


maxNumberOfQuestions : Int
maxNumberOfQuestions =
    5
