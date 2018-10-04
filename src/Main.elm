module Main exposing (main)

import Browser
import Html
import State exposing (init, subscriptions, update)
import Types exposing (Model, Msg)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = subscriptions
        }
