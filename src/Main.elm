module Main exposing (main)

import Browser
import Html
import State exposing (init, subscriptions, update)
import Types exposing (Model, Msg)


main : Program () Model Msg
main =
    Browser.element
        { view = always <| Html.text "Hello World"
        , init = always init
        , update = update
        , subscriptions = subscriptions
        }
