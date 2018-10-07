module Styles exposing
    ( darkButton
    , dialog
    , footer
    , gitHubIcon
    , h1
    , h2
    , lightButton
    , link
    , opacity
    , page
    , score
    , settings
    , smallText
    , subTitle
    , userSelectNone
    , verySmallText
    )

import Element exposing (Attribute)
import Html.Attributes


link : List (Attribute msg)
link =
    []


gitHubIcon : List (Attribute msg)
gitHubIcon =
    []


footer : List (Attribute msg)
footer =
    []


settings : List (Attribute msg)
settings =
    []


subTitle : List (Attribute msg)
subTitle =
    []


score : List (Attribute msg)
score =
    []


h2 : List (Attribute msg)
h2 =
    []


h1 : List (Attribute msg)
h1 =
    []


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
