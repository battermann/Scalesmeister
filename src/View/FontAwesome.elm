module View.FontAwesome exposing (..)

import Html exposing (div, i)
import Html.Attributes
import Element exposing (html, el)
import Styles exposing (..)
import Element.Attributes exposing (..)


toEl iconStr =
    el None [ center, verticalCenter ] (html (div [] [ i [ Html.Attributes.class iconStr ] [] ]))


download =
    toEl "fas fa-download"


play =
    toEl "fas fa-play"


stop =
    toEl "fas fa-pause"


github =
    toEl "fab fa-github"


angleLeft =
    toEl "fas fa-angle-left"


angleRight =
    toEl "fas fa-angle-right"


doubleAngleRight =
    toEl "fas fa-angle-double-right"


doubleAngleLeft =
    toEl "fas fa-angle-double-left"


spinner =
    toEl "fas fa-spinner fa-spin"
