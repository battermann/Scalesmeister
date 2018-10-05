module Styles exposing (lightButton, opacity, page, smallText, userSelectNone)

import Element exposing (Attribute)
import Html.Attributes


lightButton : List (Attribute msg)
lightButton =
    []


page : List (Attribute msg)
page =
    []


smallText : List (Attribute msg)
smallText =
    []


opacity : Float -> Attribute msg
opacity value =
    Element.htmlAttribute <| Html.Attributes.style "opacity" (String.fromFloat value)


userSelectNone : Attribute msg
userSelectNone =
    Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
