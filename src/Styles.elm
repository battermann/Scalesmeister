module Styles exposing (..)

import Style
import Style.Color as Color exposing (..)
import Style.Font as Font
import Style.Border as Border
import Color exposing (..)
import Element.Attributes exposing (inlineStyle)


type MyStyles
    = Page
    | Title
    | Score
    | Button
    | Footer
    | Subtitle
    | LargeFontButton
    | Dialog
    | DialogBox
    | None


userSelectNone =
    inlineStyle [ ( "user-select", "none" ) ]


font : Style.Property class variation
font =
    Font.typeface
        [ Font.font "Source Sans Pro"
        , Font.font "Trebuchet MS"
        , Font.font "Lucida Grande"
        , Font.font "Bitstream Vera Sans"
        , Font.font "Helvetica Neue"
        , Font.font "sans-serif"
        ]


buttonStyle : List (Style.Property class variation)
buttonStyle =
    [ Color.background (grayscale 0.4)
    , Color.text white
    , Border.rounded 4
    ]


stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style
            Page
            [ Color.text (grayscale 0.1)
            , Color.background (grayscale 0.6)
            , font
            , Font.size 18
            ]
        , Style.style Title
            [ Font.size 60
            ]
        , Style.style Button buttonStyle
        , Style.style Score [ Color.background white ]
        , Style.style Footer [ Font.size 14 ]
        , Style.style Subtitle [ Font.light, Font.size 20 ]
        , Style.style LargeFontButton
            ((Font.size 40) :: buttonStyle)
        , Style.style Dialog
            [ Color.background (rgba 0 0 0 0.8)
            , Color.text (greyscale 0.1)
            , font
            , Font.size 18
            ]
        , Style.style DialogBox
            [ Color.background (grayscale 0.6)
            , Border.rounded 4
            ]
        ]
