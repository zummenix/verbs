module Main exposing (..)

import Html.App
import Html exposing (div, text, form, input, node, Html, Attribute)
import Html.Events exposing (onInput, on, keyCode)
import Html.Attributes exposing (placeholder, value, rel, href, class)
import Json.Decode as Json
import Time
import Random
import Task
import String
import Verbs
import TextField
import Game


css : String -> Html a
css path =
    node "link" [ rel "stylesheet", href path ] []


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.map tagger keyCode)


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type State
    = Active
    | Error


type alias Model =
    { verbs : List String
    , repetitions : List String
    , current : Maybe String
    , textInput : String
    , position : Int
    , state : State
    }


type Msg
    = InitialSeed Random.Seed
    | UpdateTextInput String
    | ValidateInput
    | NoOp


init : ( Model, Cmd Msg )
init =
    let
        model =
            { verbs = []
            , repetitions = []
            , current = Nothing
            , textInput = ""
            , position = 1
            , state = Active
            }
    in
        ( model, Task.perform identity (\time -> floor time |> Random.initialSeed |> InitialSeed) Time.now )


initWithVerbs : List String -> ( Model, Cmd Msg )
initWithVerbs verbs =
    let
        model =
            { verbs = verbs
            , repetitions = []
            , current = List.head verbs
            , textInput = ""
            , position = 1
            , state = Active
            }
    in
        model ! []


view : Model -> Html Msg
view model =
    case model.current of
        Just current ->
            let
                rightAnswer =
                    case model.state of
                        Active ->
                            []

                        Error ->
                            [ div [ class "correct_answer" ] [ text current ] ]

                inputClasses =
                    case model.state of
                        Active ->
                            "field"

                        Error ->
                            "field border_red"
            in
                div [ class "container" ]
                    ([ css "styles/styles.css"
                     , div [ class "item remaining" ] [ text ("Remaining verbs to learn: " ++ (remainingText model)) ]
                     , div [ class "item position" ] [ text (positionText model) ]
                     , div [ class "item question" ] [ text (current |> separate |> infinitive) ]
                     , div [ class "item" ]
                        [ input [ class inputClasses, onInput UpdateTextInput, onEnter ValidateInput, value model.textInput ] []
                        ]
                     ]
                        ++ rightAnswer
                    )

        Nothing ->
            div [ class "container" ]
                [ css "styles/styles.css"
                , div [ class "finish" ] [ text "Congratulations!" ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialSeed seed ->
            initWithVerbs (Verbs.shuffled seed)

        UpdateTextInput text ->
            { model | textInput = text } ! []

        ValidateInput ->
            nextVerb model ! []

        NoOp ->
            model ! []


nextVerb : Model -> Model
nextVerb model =
    if isAnswerValid model then
        case model.state of
            Active ->
                if model.position == numberOfQuestions model then
                    let
                        verbs =
                            model.repetitions ++ (Maybe.withDefault [] (List.tail model.verbs))
                    in
                        { model
                            | verbs = verbs
                            , repetitions = []
                            , current = List.head verbs
                            , textInput = ""
                            , position = 1
                        }
                else
                    let
                        verbs =
                            Maybe.withDefault [] (List.tail model.verbs)
                    in
                        { model
                            | verbs = verbs
                            , current = List.head verbs
                            , textInput = ""
                            , position = model.position + 1
                        }

            Error ->
                if model.position == numberOfQuestions model then
                    let
                        repetitions =
                            model.repetitions ++ (Maybe.withDefault [] (Maybe.map (\w -> [ w ]) model.current))

                        verbs =
                            repetitions ++ (Maybe.withDefault [] (List.tail model.verbs))
                    in
                        { model
                            | verbs = verbs
                            , repetitions = []
                            , current = List.head verbs
                            , textInput = ""
                            , position = 1
                            , state = Active
                        }
                else
                    let
                        repetitions =
                            model.repetitions ++ (Maybe.withDefault [] (Maybe.map (\w -> [ w ]) model.current))

                        verbs =
                            Maybe.withDefault [] (List.tail model.verbs)
                    in
                        { model
                            | verbs = verbs
                            , repetitions = repetitions
                            , current = List.head verbs
                            , textInput = ""
                            , position = model.position + 1
                            , state = Active
                        }
    else
        case model.state of
            Active ->
                { model | state = Error }

            Error ->
                model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


positionText : Model -> String
positionText model =
    (model.position |> toString) ++ " of " ++ (numberOfQuestions model |> toString)


remainingText : Model -> String
remainingText model =
    ((List.length model.verbs) + (List.length model.repetitions)) |> toString


numberOfQuestions : Model -> Int
numberOfQuestions model =
    min 7 (List.length model.verbs)


isAnswerValid : Model -> Bool
isAnswerValid model =
    (separate (Maybe.withDefault "" model.current)) == (separate model.textInput)


separate : String -> List (List String)
separate text =
    List.map separateSlash (String.split "," text)


separateSlash : String -> List String
separateSlash text =
    List.sort (List.map String.trim (String.split "/" text))


infinitive : List (List String) -> String
infinitive words =
    Maybe.withDefault "" (List.head (Maybe.withDefault [] (List.head words)))
