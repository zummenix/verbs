module TextField exposing (Config, State(..), view)

import Html exposing (div, text, p, input, Html, Attribute)
import Html.Events exposing (onInput, on, keyCode)
import Html.Attributes exposing (id, value, style, disabled)
import Json.Decode


type State
    = Normal
    | Correct
    | Wrong
    | Static


type alias Config msg =
    { onInput : String -> msg
    , onKeyUp : Int -> msg
    }


view : String -> Config msg -> State -> String -> Html msg
view fieldID config state text =
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
                , ( "font-family", "sans-serif" )
                ]
            , id fieldID
            , onInput config.onInput
            , onKeyUp config.onKeyUp
            , value text
            , disabled (userInteractionDisabled state)
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

        Static ->
            "rgb(250,241,192)"


borderColor : State -> String
borderColor state =
    case state of
        Normal ->
            "lightgray"

        Correct ->
            "rgb(0,220,0)"

        Wrong ->
            "rgb(220,0,0)"

        Static ->
            "lightgray"


userInteractionDisabled : State -> Bool
userInteractionDisabled state =
    case state of
        Static ->
            True

        _ ->
            False


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.Decode.map tagger keyCode)
