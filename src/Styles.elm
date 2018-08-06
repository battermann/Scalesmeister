module Styles exposing (userSelectNone, stylesheet, AppStyles(..))

import Style
import Style.Color as Color
import Style.Font as Font
import Color exposing (white, grayscale, rgba)
import Element.Attributes exposing (inlineStyle)


type AppStyles
    = Page
    | H1
    | H2
    | Score
    | Button
    | Footer
    | Subtitle
    | DarkButton
    | Dialog
    | DialogBox
    | Link
    | GitHubIcon
    | SkipsNSteps
    | SmallText
    | VerySmallText
    | RangeButton
    | Settings
    | LightButton
    | None


userSelectNone =
    inlineStyle [ ( "user-select", "none" ) ]


font : Style.Property class variation
font =
    Font.typeface
        [ Font.font "Raleway"
        , Font.font "sans-serif"
        ]


buttonStyle : List (Style.Property class variation)
buttonStyle =
    [ Color.background (grayscale 0.4)
    , Color.text white
    ]


stylesheet : Style.StyleSheet AppStyles variation
stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style
            Page
            [ Color.text (grayscale 0.1)
            , Color.background (grayscale 0.6)
            , font
            , Font.size 18
            , Font.weight 300
            ]
        , Style.style H1
            [ Font.size 60
            ]
        , Style.style H2
            [ Font.size 40
            ]
        , Style.style Button buttonStyle
        , Style.style LightButton
            [ Color.background (grayscale 0.05)
            , Color.text (grayscale 0.6)
            ]
        , Style.style Score [ Color.background white ]
        , Style.style Footer [ Font.size 16 ]
        , Style.style Subtitle [ Font.light, Font.size 20 ]
        , Style.style DarkButton
            [ Color.background (grayscale 0.75)
            , Color.text (grayscale 0.1)
            ]
        , Style.style Dialog
            [ Color.background (rgba 0 0 0 0.8)
            , Color.text (grayscale 0.1)
            , font
            , Font.size 18
            ]
        , Style.style DialogBox
            [ Color.background (grayscale 0.6)
            ]
        , Style.style Link
            [ Font.underline
            ]
        , Style.style SkipsNSteps
            (Font.size 20 :: buttonStyle)
        , Style.style GitHubIcon
            [ Font.size 30
            ]
        , Style.style SmallText
            [ Font.size 14
            ]
        , Style.style VerySmallText
            [ Font.size 11
            ]
        , Style.style Settings
            [ Color.background (grayscale 0.75)
            ]
        , Style.style RangeButton
            (buttonStyle
                ++ [ Font.size 12
                   , Color.background (grayscale 0.75)
                   , Color.text (grayscale 0.35)
                   ]
            )
        ]
