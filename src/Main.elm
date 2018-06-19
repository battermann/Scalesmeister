module Main exposing (..)

import Html
import Update exposing (..)
import Model exposing (..)
import View


main : Program Never Model Msg
main =
    Html.program
        { view = View.view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
