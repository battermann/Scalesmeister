module Main exposing (main)

import Html
import State exposing (init, update)
import Types exposing (Model, Msg)
import View


main : Program Never Model Msg
main =
    Html.program
        { view = View.view
        , init = init
        , update = update
        , subscriptions = State.subscriptions
        }
