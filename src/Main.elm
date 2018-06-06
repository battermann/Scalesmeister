module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, p)
import Html.Events exposing (onMouseDown, onMouseUp)
import Ports exposing (..)
import Types exposing (..)

---- MODEL ----

loadNoteSample : Note -> Cmd msg
loadNoteSample note =
    let
        url = "samples/" ++ (toString note) ++ ".mp3"
    in
        loadSample (toString note, url)

loadSamples : Cmd msg
loadSamples =
    Cmd.batch (List.map loadNoteSample [C, DSharp, FSharp, A])


init : ( Model, Cmd Msg )
init =
    ( Nothing, loadSamples )



---- UPDATE ----


type Msg
    = NoteOn Note
    | NoteOff


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteOn note -> ( Just note, noteOn (toString note))

        NoteOff -> ( Nothing, noteOff ())



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onMouseDown (NoteOn C), onMouseUp NoteOff ] [ text "C" ]
        , button [ onMouseDown (NoteOn DSharp), onMouseUp NoteOff ] [ text "D#" ]
        , button [ onMouseDown (NoteOn FSharp), onMouseUp NoteOff ] [ text "F#" ]
        , button [ onMouseDown (NoteOn A), onMouseUp NoteOff ] [ text "A" ]
        , p [] [ text (Maybe.withDefault "" (Maybe.map toString model)) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
