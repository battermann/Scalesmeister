module View.Styles exposing
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
    , rangeTrack
    , score
    , settings
    , smallText
    , subTitle
    , userSelectNone
    , verySmallText
    )

import Element exposing (Attribute, rgb, rgba)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes


rangeTrack : List (Attribute msg)
rangeTrack =
    [ Background.color veryLightGray ]


font : Attribute msg
font =
    Font.family
        [ Font.typeface "Raleway"
        , Font.sansSerif
        ]


lightGray : Element.Color
lightGray =
    rgb 0.4 0.4 0.4


darkGray : Element.Color
darkGray =
    rgb 0.25 0.25 0.25


veryLightGray : Element.Color
veryLightGray =
    rgb 0.9 0.9 0.9


link : List (Attribute msg)
link =
    [ Font.underline ]


gitHubIcon : List (Attribute msg)
gitHubIcon =
    [ Font.size 30 ]


footer : List (Attribute msg)
footer =
    [ Font.size 16 ]


settings : List (Attribute msg)
settings =
    [ Background.color darkGray ]


subTitle : List (Attribute msg)
subTitle =
    [ Font.size 20, Font.center ]


score : List (Attribute msg)
score =
    [ Background.color veryLightGray ]


h2 : List (Attribute msg)
h2 =
    [ Font.size 40 ]


h1 : List (Attribute msg)
h1 =
    [ Font.size 60 ]


lightButton : List (Attribute msg)
lightButton =
    [ Background.color veryLightGray
    , Font.color lightGray
    ]


darkButton : List (Attribute msg)
darkButton =
    [ Background.color darkGray
    , Font.color veryLightGray
    ]


page : List (Attribute msg)
page =
    [ Background.color lightGray
    , Font.color veryLightGray
    , Element.htmlAttribute <| Html.Attributes.style "font-weight" "300"
    , font
    ]


smallText : List (Attribute msg)
smallText =
    [ Font.size 14 ]


dialog : List (Attribute msg)
dialog =
    [ Background.color <| rgba 0 0 0 0.8
    ]


verySmallText : List (Attribute msg)
verySmallText =
    [ Font.size 11 ]


opacity : Float -> Attribute msg
opacity value =
    Element.htmlAttribute <| Html.Attributes.style "opacity" (String.fromFloat value)


userSelectNone : Attribute msg
userSelectNone =
    Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
