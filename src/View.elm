module View exposing (view)

import Html exposing (Html)
import Types exposing (Model, Msg(..))
import View.FontAwesome as FontAwesome
import View.RangeInput as RangeInput


view : Model -> Html Msg
view _ =
    Html.text "Hello World"
