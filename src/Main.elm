module Main exposing (Field(..), Model, Msg(..), Status(..), answers, emptyFields, fieldID, fields, focusOnFirstEmptyField, init, main, subscriptions, update, view, viewActionButton, viewAnswer, viewCorrectAnswer, viewField, viewHelperText, viewWrongAnswer)

import Dom
import Game exposing (Game)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
import Html.Events
import Keyboard
import Question
import Random
import String
import Task
import TextField
import Time
import Verbs


main : Program Never Model Msg
main =
    Html.program
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
    | KeyboardEnter
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( { game = Game.initGame [], fields = [], status = Ready }
    , Task.perform (\time -> floor time |> Random.initialSeed |> InitialSeed) Time.now
    )


view : Model -> Html Msg
view model =
    div
        [ style "margin" "auto"
        , style "width" "280px"
        , style "height" "auto"
        ]
        (case model.status of
            Ready ->
                List.indexedMap viewField model.fields ++ [ viewActionButton "Check" Validate ]

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
        [ style "text-align" "center"
        , style "color" "gray"
        , style "font-family" "sans-serif"
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
        [ style "text-align" "center"
        , style "color" color
        , style "font-family" "sans-serif"
        ]
        [ Html.strong [] [ text answer ] ]


viewActionButton : String -> Msg -> Html Msg
viewActionButton title action =
    Html.button
        [ style "border" "0"
        , style "margin" "0 10px"
        , style "float" "right"
        , style "cursor" "pointer"
        , style "font-size" "1.1em"
        , style "font-weight" "200"
        , style "background-color" "#0095dd"
        , style "color" "#fff"
        , style "text-transform" "uppercase"
        , style "padding" "10px 30px"
        , style "border-radius" "4px"
        , style "box-shadow" "inset 0 -1px #bbbfc2"
        , Html.Events.onClick action
        ]
        [ text title ]


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
                { onInput = OnInput index, onKeyUp = OnKeyUp index }
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
            ( { game = model.game, fields = newFields, status = model.status }
            , Cmd.none
            )

        OnKeyUp index code ->
            if code == 13 then
                -- Enter
                focusOnFirstEmptyField model

            else
                ( model
                , Cmd.none
                )

        Validate ->
            case Game.currentRoundQuestion model.game of
                Just question ->
                    let
                        isCorrect =
                            Question.validate (Question.words question) (answers model.fields)
                    in
                    if isCorrect then
                        ( { model | status = Success question }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | status = Error question (answers model.fields)
                            , game = Game.addToRepeat question model.game
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        Next ->
            let
                game =
                    Game.nextRoundQuestion model.game
            in
            focusOnFirstEmptyField { game = game, fields = fields game, status = Ready }

        KeyboardEnter ->
            case model.status of
                Ready ->
                    ( model
                    , Cmd.none
                    )

                Error _ _ ->
                    ( model, Task.perform identity (Task.succeed Next) )

                Success _ ->
                    ( model, Task.perform identity (Task.succeed Next) )

        NoOp ->
            ( model
            , Cmd.none
            )


focusOnFirstEmptyField : Model -> ( Model, Cmd Msg )
focusOnFirstEmptyField model =
    case List.head (emptyFields model) of
        Just i ->
            ( model, Task.attempt (\_ -> NoOp) (Dom.focus (fieldID i)) )

        Nothing ->
            ( model, Task.perform identity (Task.succeed Validate) )


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
    "field-" ++ toString index


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups
        (\code ->
            if code == 13 then
                KeyboardEnter

            else
                NoOp
        )
