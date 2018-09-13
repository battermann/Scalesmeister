module View.RangeInput exposing (input)

import Element exposing (Element)
import Html
import Html.Attributes
import Html.Events


input : Float -> (String -> msg) -> Element style variation msg
input value updateValue =
    Element.html <|
        Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "60"
            , Html.Attributes.max "220"
            , Html.Attributes.value <| toString value
            , Html.Events.onInput updateValue
            , Html.Attributes.style [ ( "width", "100%" ), ( "pointer-events", "auto" ) ]
            ]
            []
