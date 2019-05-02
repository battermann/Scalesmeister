module View.FontAwesome exposing
    ( angleDown
    , angleLeft
    , angleRight
    , angleUp
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

import Element exposing (centerX, centerY, el, html)
import Html exposing (div, i)
import Html.Attributes exposing (class)


toEl : String -> Element.Element msg
toEl iconStr =
    el [ centerX, centerY ] (html (div [] [ i [ class iconStr ] [] ]))


volumeUp : Element.Element msg
volumeUp =
    toEl "fas fa-volume-up"


volumeOff : Element.Element msg
volumeOff =
    toEl "fas fa-volume-off"


download : Element.Element msg
download =
    toEl "fas fa-download"


play : Element.Element msg
play =
    toEl "fas fa-play"


stop : Element.Element msg
stop =
    toEl "fas fa-pause"


github : Element.Element msg
github =
    toEl "fab fa-github"


angleLeft : Element.Element msg
angleLeft =
    toEl "fas fa-angle-left"


angleRight : Element.Element msg
angleRight =
    toEl "fas fa-angle-right"


doubleAngleRight : Element.Element msg
doubleAngleRight =
    toEl "fas fa-angle-double-right"


doubleAngleLeft : Element.Element msg
doubleAngleLeft =
    toEl "fas fa-angle-double-left"


angleDown : Element.Element msg
angleDown =
    toEl "fas fa-angle-down"


angleUp : Element.Element msg
angleUp =
    toEl "fas fa-angle-up"


spinner : Element.Element msg
spinner =
    toEl "fas fa-spinner fa-spin"
