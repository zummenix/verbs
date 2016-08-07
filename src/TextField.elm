module TextField exposing (Msg, init, view, update)

import Html.App
import Html exposing (div, text, p, input, Html, Attribute)
import Html.Events exposing (onInput, on, keyCode)
import Html.Attributes exposing (value, style)
import Json.Decode


main : Program Never
main =
    Html.App.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


type State
    = Normal
    | Correct
    | Wrong


type alias Model =
    { input : String
    , state : State
    }


type Msg
    = NoOp
    | Update String


init : ( Model, Cmd Msg )
init =
    { input = "", state = Normal } ! []


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "10px" )
            , ( "padding", "5px" )
            , ( "background-color", backgroundColor model.state )
            , ( "border-color", borderColor model.state )
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
            "rgb(0,240,0)"

        Wrong ->
            "rgb(240,0,0)"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Update input ->
            { model | input = input } ! []


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.Decode.map tagger keyCode)
