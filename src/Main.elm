module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, p)
import Html.Events exposing (onMouseDown, onMouseUp)
import Ports exposing (..)
import Types exposing (..)
import Pitches exposing (..)

---- MODEL ----


toPitchNotation : Pitch -> PitchNotation
toPitchNotation pitch =
    let
        note = toString pitch.note
        accidental =
            case pitch.accidental of
                Just Sharp -> "#"
                Just Flat -> "b"
                Nothing -> ""
        octave = toString pitch.octave

    in
        note ++ accidental ++ octave

pitchSampleUrlMapping : Pitch -> ( PitchNotation, SampleUrl )
pitchSampleUrlMapping pitch =
    let
        note = toString pitch.note
        accidental =
            Maybe.map toString pitch.accidental
            |> Maybe.withDefault ""
        octave = toString pitch.octave
        url = "samples/" ++ note ++ accidental ++ octave ++ ".mp3"
    in
        ( toPitchNotation pitch, url )

loadPianoSamples : Cmd msg
loadPianoSamples =
    loadSamples ( List.map pitchSampleUrlMapping [ c4, dSharp4, fSharp4, a4 ] )


init : ( Model, Cmd Msg )
init =
    ( Nothing, loadPianoSamples )



---- UPDATE ----


type Msg
    = NoteOn Pitch
    | NoteOff Pitch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteOn pitch -> ( Just pitch, noteOn (toPitchNotation pitch))

        NoteOff pitch -> ( Nothing, noteOff (toPitchNotation pitch))



---- VIEW ----


keys : List (Html Msg)
keys =
    chromaticScaleFromC4ToB
    |> List.map (\p -> button [ onMouseDown (NoteOn p), onMouseUp (NoteOff p) ] [ text (toPitchNotation p) ])

view : Model -> Html Msg
view model =
    div []
        (keys ++ [ p [] [ text (Maybe.withDefault "" (Maybe.map toPitchNotation model)) ] ])


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
