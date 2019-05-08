module Main exposing (main)

import Browser
import State exposing (init, subscriptions, update)
import Types exposing (Model, Msg(..))
import View exposing (view)


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = always init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
