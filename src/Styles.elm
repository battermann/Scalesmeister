module Styles exposing (darkButton, dialog, lightButton, opacity, page, smallText, userSelectNone, verySmallText)

import Element exposing (Attribute)
import Html.Attributes


lightButton : List (Attribute msg)
lightButton =
    []


darkButton : List (Attribute msg)
darkButton =
    []


page : List (Attribute msg)
page =
    []


smallText : List (Attribute msg)
smallText =
    []


dialog : List (Attribute msg)
dialog =
    []


verySmallText : List (Attribute msg)
verySmallText =
    []


opacity : Float -> Attribute msg
opacity value =
    Element.htmlAttribute <| Html.Attributes.style "opacity" (String.fromFloat value)


userSelectNone : Attribute msg
userSelectNone =
    Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
