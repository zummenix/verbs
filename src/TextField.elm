module TextField exposing (Config, State(..), view)

import Html exposing (Attribute, Html, div, input, p, text)
import Html.Attributes exposing (disabled, id, style, value)
import Html.Events exposing (keyCode, on, onInput)
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
        [ style "margin" "10px"
        , style "padding" "5px"
        , style "background-color" (backgroundColor state)
        , style "border-color" (borderColor state)
        , style "box-shadow" "inset 0px 1px 3px 3px rgba(0,0,0,0.02)"
        , style "border-style" "solid"
        , style "border-radius" "4px"
        , style "border-width" "1px"
        ]
        [ input
            [ style "padding" "5px"
            , style "width" "100%"
            , style "box-sizing" "border-box"
            , style "text-align" "center"
            , style "outline" "none"
            , style "border" "none"
            , style "background-color" "transparent"
            , style "color" "rgb(44,44,44)"
            , style "font-size" "1.2em"
            , style "font-family" "sans-serif"
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
