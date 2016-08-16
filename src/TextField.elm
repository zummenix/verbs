module TextField exposing (Msg, State, init, view, update)

import Html.App
import Html exposing (div, text, p, input, Html, Attribute)
import Html.Events exposing (onInput, on, keyCode)
import Html.Attributes exposing (value, style)
import Json.Decode


main : Program Never
main =
    Html.App.program { init = init, view = view Correct, update = update, subscriptions = \_ -> Sub.none }


type State
    = Normal
    | Correct
    | Wrong


type alias Model =
    { input : String }


type Msg
    = Update String
    | KeyUp Int


init : ( Model, Cmd Msg )
init =
    { input = "" } ! []


view : State -> Model -> Html Msg
view state model =
    div
        [ style
            [ ( "margin", "10px" )
            , ( "padding", "5px" )
            , ( "background-color", backgroundColor state )
            , ( "border-color", borderColor state )
            , ( "box-shadow", "inset 0px 1px 3px 3px rgba(0,0,0,0.02)" )
            , ( "border-style", "solid" )
            , ( "border-radius", "4px" )
            , ( "border-width", "1px" )
            ]
        ]
        [ input
            [ style
                [ ( "padding", "5px" )
                , ( "width", "100%" )
                , ( "box-sizing", "border-box" )
                , ( "text-align", "center" )
                , ( "outline", "none" )
                , ( "border", "none" )
                , ( "background-color", "transparent" )
                , ( "color", "rgb(44,44,44)" )
                , ( "font-size", "1.2em" )
                ]
            , onInput Update
            , onKeyUp KeyUp
            , value model.input
            ]
            []
        ]


backgroundColor : State -> String
backgroundColor state =
    case state of
        Normal ->
            "rgb(252,252,252)"

        Correct ->
            "rgba(0,240,0,0.05)"

        Wrong ->
            "rgba(240,0,0,0.05)"


borderColor : State -> String
borderColor state =
    case state of
        Normal ->
            "lightgray"

        Correct ->
            "rgb(0,220,0)"

        Wrong ->
            "rgb(220,0,0)"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update input ->
            { model | input = input } ! []

        KeyUp _ ->
            model ! []


onKeyUp : (Int -> Msg) -> Attribute Msg
onKeyUp tagger =
    on "keyup" (Json.Decode.map tagger keyCode)
