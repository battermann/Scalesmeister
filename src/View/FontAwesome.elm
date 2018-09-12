module View.FontAwesome exposing
    ( angleLeft
    , angleRight
    , doubleAngleLeft
    , doubleAngleRight
    , download
    , github
    , play
    , spinner
    , stop
    , volumeOff
    , volumeUp
    )

import Element exposing (el, html)
import Element.Attributes exposing (center, verticalCenter)
import Html exposing (div, i)
import Html.Attributes exposing (class)
import Styles exposing (AppStyles(..))


toEl : String -> Element.Element AppStyles variation msg
toEl iconStr =
    el None [ center, verticalCenter ] (html (div [] [ i [ class iconStr ] [] ]))


volumeUp : Element.Element AppStyles variation msg
volumeUp =
    toEl "fas fa-volume-up"


volumeOff : Element.Element AppStyles variation msg
volumeOff =
    toEl "fas fa-volume-off"


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
