module View.RangeInput exposing (input)

import Element exposing (Element)
import Html
import Html.Attributes
import Html.Events


input : Float -> (String -> msg) -> Element msg
input value updateValue =
    Element.html <|
        Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "60"
            , Html.Attributes.max "220"
            , Html.Attributes.value <| String.fromFloat value
            , Html.Events.onInput updateValue
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "pointer-events" "auto"
            , Html.Attributes.style "-webkit-appearance" "slider-horizontal"
            , Html.Attributes.style "opacity" "1"
            , Html.Attributes.style "step" "1"
            ]
            []
