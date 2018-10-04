module View exposing (view)

import Html exposing (Html)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view _ =
    Html.text "Hello World"
