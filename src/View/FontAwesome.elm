module View.FontAwesome exposing (..)

import Html exposing (div, i)
import Html.Attributes
import Element exposing (html)


toEl iconStr =
    (html (div [] [ i [ Html.Attributes.class iconStr ] [] ]))


download =
    toEl "fas fa-download"


play =
    toEl "fas fa-play"


stop =
    toEl "fas fa-stop"


arrowUp =
    toEl "fas fa-arrow-up"


arrowDown =
    toEl "fas fa-arrow-down"


github =
    toEl "fab fa-github"
