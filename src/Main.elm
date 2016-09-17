module Main exposing (..)

import Html.App
import Html exposing (div, text, Html, Attribute)
import Html.Attributes exposing (style)
import Time
import Random
import Task
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


type Model
    = Loading
    | Ready GameState
    | Finished


type alias GameState =
    { game : Game
    , fields : List Field
    }


type Field
    = Known String
    | Unknown String


type Msg
    = InitialSeed Random.Seed
    | OnInput Int String
    | OnKeyUp Int Int
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( Loading, Task.perform identity (\time -> floor time |> Random.initialSeed |> InitialSeed) Time.now )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            viewPhrase "Loading..."

        Ready gameState ->
            viewGame gameState

        Finished ->
            viewPhrase "Congratulations!"


viewGame : GameState -> Html Msg
viewGame gameState =
    div
        [ style
            [ ( "margin", "auto" )
            , ( "width", "280px" )
            , ( "height", "auto" )
            ]
        ]
        (List.indexedMap viewField gameState.fields)


viewField : Int -> Field -> Html Msg
viewField index field =
    case field of
        Known verb ->
            div
                [ style
                    [ ( "margin-top", "20px" )
                    , ( "margin-bottom", "20px" )
                    , ( "margin-left", "10px" )
                    , ( "margin-right", "10px" )
                    , ( "padding", "15px" )
                    , ( "background-color", "rgb(250,241,192)" )
                    , ( "border-color", "lightgray" )
                    , ( "border-style", "solid" )
                    , ( "border-radius", "4px" )
                    , ( "border-width", "1px" )
                    , ( "text-align", "center" )
                    , ( "color", "rgb(44,44,44)" )
                    , ( "font-size", "1.2em" )
                    , ( "font-family", "sans-serif" )
                    ]
                ]
                [ text verb ]

        Unknown text ->
            TextField.view { onInput = (OnInput index), onKeyUp = (OnKeyUp index) } TextField.Normal text


viewPhrase : String -> Html Msg
viewPhrase phrase =
    div
        [ style
            [ ( "margin", "auto" )
            , ( "width", "240px" )
            , ( "height", "auto" )
            ]
        ]
        [ div
            [ style
                [ ( "padding-top", "100%" )
                , ( "padding-bottom", "100%" )
                , ( "text-align", "center" )
                , ( "font-family", "sans-serif" )
                , ( "font-size", "2em" )
                , ( "color", "gray" )
                ]
            ]
            [ text phrase ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialSeed seed ->
            let
                game =
                    Game.initGame (Verbs.shuffled seed)
            in
                Ready { game = game, fields = fields game } ! []

        OnInput index text ->
            case model of
                Ready gameState ->
                    let
                        updateField i field =
                            if i == index then
                                Unknown text
                            else
                                field

                        newFields =
                            List.indexedMap updateField gameState.fields
                    in
                        Ready { game = gameState.game, fields = newFields } ! []

                Loading ->
                    model ! []

                Finished ->
                    model ! []

        OnKeyUp index code ->
            if code == 13 then
                -- Enter
                model ! []
            else if code == 9 then
                -- Tab
                model ! []
            else
                model ! []

        NoOp ->
            model ! []


fields : Game -> List Field
fields game =
    case Game.currentRoundQuestion game of
        Just question ->
            let
                field i text =
                    if i == 0 || text == "-" then
                        Known text
                    else
                        Unknown ""
            in
                List.indexedMap field (List.map (Maybe.withDefault "") (List.map List.head (Question.words question)))

        Nothing ->
            []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
