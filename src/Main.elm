module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, button, p)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Ports exposing (..)
import Types exposing (..)
import Pitches exposing (..)
import Random exposing (..)
import Array
import Random.Array exposing (..)


---- MODEL ----


toPitchNotation : Pitch -> PitchNotation
toPitchNotation pitch =
    let
        note =
            toString pitch.note

        accidental =
            case pitch.accidental of
                Just Sharp ->
                    "#"

                Just Flat ->
                    "b"

                Nothing ->
                    ""

        octave =
            toString pitch.octave
    in
        note ++ accidental ++ octave


pitchToSampleUrlMapping : Pitch -> ( PitchNotation, SampleUrl )
pitchToSampleUrlMapping pitch =
    let
        note =
            toString pitch.note

        accidental =
            Maybe.map toString pitch.accidental
                |> Maybe.withDefault ""

        octave =
            toString pitch.octave

        url =
            "samples/" ++ note ++ accidental ++ octave ++ ".mp3"
    in
        ( toPitchNotation pitch, url )


loadPianoSamples : Cmd msg
loadPianoSamples =
    [ c4, dSharp4, fSharp4, a4 ]
        |> List.map pitchToSampleUrlMapping
        |> loadSamples


startRnd12ToneSeq : Cmd Msg
startRnd12ToneSeq =
    let
        arr =
            Array.fromList chromaticScaleFromC4ToB
    in
        Random.generate (Array.toList >> StartSequence) (Random.Array.shuffle arr)


init : ( Model, Cmd Msg )
init =
    ( Nothing, loadPianoSamples )



---- UPDATE ----


type Msg
    = NoteOn Pitch
    | NoteOff Pitch
    | StartSequence (List Pitch)
    | StopSequence
    | StartRandom12ToneSequence


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteOn pitch ->
            ( Just pitch, noteOn (toPitchNotation pitch) )

        NoteOff pitch ->
            ( Nothing, noteOff (toPitchNotation pitch) )

        StartSequence seq ->
            ( model, startSequence (List.map toPitchNotation seq) )

        StopSequence ->
            ( model, stopSequence () )

        StartRandom12ToneSequence ->
            ( model, startRnd12ToneSeq )



---- VIEW ----


keys : List (Html Msg)
keys =
    chromaticScaleFromC4ToB
        |> List.map (\p -> button [ onMouseDown (NoteOn p), onMouseUp (NoteOff p) ] [ text (toPitchNotation p) ])


displayPitch : Model -> Html Msg
displayPitch model =
    p [] [ text (Maybe.withDefault "" (Maybe.map toPitchNotation model)) ]


startSequenceButton : Html Msg
startSequenceButton =
    button [ onClick StartRandom12ToneSequence ] [ text "Start" ]


stopSequenceButton : Html Msg
stopSequenceButton =
    button [ onClick StopSequence ] [ text "Stop" ]


view : Model -> Html Msg
view model =
    div []
        ([ h1 [] [ text "luigi" ]
         , h2 [] [ text "random 12 tone sequence" ]
         , startSequenceButton
         , stopSequenceButton
         , h2 [] [ text "play back samples" ]
         ]
            ++ keys
            ++ [ displayPitch model ]
        )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
