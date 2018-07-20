module View.FontAwesome exposing (..)

import Html exposing (div, i)
import Html.Attributes
import Element exposing (html, el)
import Styles exposing (..)
import Element.Attributes exposing (..)


toEl : String -> Element.Element AppStyles variation msg
toEl iconStr =
    el None [ center, verticalCenter ] (html (div [] [ i [ Html.Attributes.class iconStr ] [] ]))


download : Element.Element AppStyles variation msg
download =
    toEl "fas fa-download"


play : Element.Element AppStyles variation msg
play =
    toEl "fas fa-play"


stop : Element.Element AppStyles variation msg
stop =
    toEl "fas fa-pause"


github : Element.Element AppStyles variation msg
github =
    toEl "fab fa-github"


angleLeft : Element.Element AppStyles variation msg
angleLeft =
    toEl "fas fa-angle-left"


angleRight : Element.Element AppStyles variation msg
angleRight =
    toEl "fas fa-angle-right"


doubleAngleRight : Element.Element AppStyles variation msg
doubleAngleRight =
    toEl "fas fa-angle-double-right"


doubleAngleLeft : Element.Element AppStyles variation msg
doubleAngleLeft =
    toEl "fas fa-angle-double-left"


spinner : Element.Element AppStyles variation msg
spinner =
    toEl "fas fa-spinner fa-spin"
