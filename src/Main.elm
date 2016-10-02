module Main exposing (..)

import Html.App
import Html exposing (div, text, Html, Attribute)
import Html.Attributes exposing (style)
import Html.Events
import String
import Time
import Random
import Task
import Dom
import Verbs
import TextField
import Game exposing (Game)
import Question


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { game : Game
    , fields : List Field
    , status : Status
    }


type Field
    = Known String
    | Unknown String


type Status
    = Ready
    | Error String (List String)
    | Success String


type Msg
    = InitialSeed Random.Seed
    | OnInput Int String
    | OnKeyUp Int Int
    | Validate
    | Next
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( { game = Game.initGame [], fields = [], status = Ready }
    , Task.perform identity (\time -> floor time |> Random.initialSeed |> InitialSeed) Time.now
    )


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "auto" )
            , ( "width", "280px" )
            , ( "height", "auto" )
            ]
        ]
        (case model.status of
            Ready ->
                ((List.indexedMap viewField model.fields) ++ [ viewActionButton "Check" Validate ])

            Error correct answers ->
                [ viewHelperText "You've answered:"
                , viewWrongAnswer (String.join ", " answers)
                , viewHelperText "but correct is:"
                , viewCorrectAnswer correct
                , viewActionButton "Next" Next
                ]

            Success correct ->
                [ viewHelperText "You're correct:"
                , viewCorrectAnswer correct
                , viewActionButton "Next" Next
                ]
        )


viewHelperText : String -> Html Msg
viewHelperText line =
    Html.p
        [ style
            [ ( "text-align", "center" )
            , ( "color", "gray" )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ Html.em [] [ text line ] ]


viewCorrectAnswer : String -> Html Msg
viewCorrectAnswer =
    viewAnswer "green"


viewWrongAnswer : String -> Html Msg
viewWrongAnswer =
    viewAnswer "red"


viewAnswer : String -> String -> Html Msg
viewAnswer color answer =
    Html.p
        [ style
            [ ( "text-align", "center" )
            , ( "color", color )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ Html.strong [] [ text answer ] ]


viewActionButton : String -> Msg -> Html Msg
viewActionButton title action =
    Html.p
        [ style
            [ ( "text-align", "right" )
            , ( "margin", "10px" )
            , ( "color", "gray" )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ Html.a
            [ Html.Events.onClick action, Html.Attributes.href "#" ]
            [ text title ]
        ]


viewField : Int -> Field -> Html Msg
viewField index field =
    case field of
        Known verb ->
            TextField.view
                (fieldID index)
                { onInput = \_ -> NoOp, onKeyUp = \_ -> NoOp }
                TextField.Static
                verb

        Unknown verb ->
            TextField.view
                (fieldID index)
                { onInput = (OnInput index), onKeyUp = (OnKeyUp index) }
                TextField.Normal
                verb


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialSeed seed ->
            let
                game =
                    Game.initGame (Verbs.shuffled seed)
            in
                focusOnFirstEmptyField { game = game, fields = fields game, status = model.status }

        OnInput index text ->
            let
                updateField i field =
                    if i == index then
                        Unknown text
                    else
                        field

                newFields =
                    List.indexedMap updateField model.fields
            in
                { game = model.game, fields = newFields, status = model.status } ! []

        OnKeyUp index code ->
            if code == 13 then
                -- Enter
                focusOnFirstEmptyField model
            else
                model ! []

        Validate ->
            case Game.currentRoundQuestion model.game of
                Just question ->
                    let
                        isCorrect =
                            Question.validate (Question.words question) (answers model.fields)
                    in
                        if isCorrect then
                            { model | status = Success question } ! []
                        else
                            { model
                                | status = Error question (answers model.fields)
                                , game = Game.addToRepeat question model.game
                            }
                                ! []

                Nothing ->
                    model ! []

        Next ->
            let
                game =
                    Game.nextRoundQuestion model.game
            in
                focusOnFirstEmptyField { game = game, fields = fields game, status = Ready }

        NoOp ->
            model ! []


focusOnFirstEmptyField : Model -> ( Model, Cmd Msg )
focusOnFirstEmptyField model =
    case List.head (emptyFields model) of
        Just i ->
            ( model, Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus (fieldID i)) )

        Nothing ->
            ( model, Task.perform identity identity (Task.succeed Validate) )


fields : Game -> List Field
fields game =
    case Game.currentRoundQuestion game of
        Just question ->
            let
                field i verb =
                    if i == 0 || verb == "-" then
                        Known verb
                    else
                        Unknown ""
            in
                List.indexedMap field (List.map (Maybe.withDefault "") (List.map List.head (Question.words question)))

        Nothing ->
            []


emptyFields : Model -> List Int
emptyFields gameState =
    let
        check index field =
            case field of
                Known _ ->
                    Nothing

                Unknown verb ->
                    if String.isEmpty verb then
                        Just index
                    else
                        Nothing
    in
        List.filterMap identity (List.indexedMap check gameState.fields)


answers : List Field -> List String
answers fields =
    let
        mapper field =
            case field of
                Known text ->
                    text

                Unknown text ->
                    text
    in
        List.map mapper fields


fieldID : Int -> String
fieldID index =
    ("field-" ++ (toString index))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
