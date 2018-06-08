module Main exposing (..)

import Html exposing (Html, text, div, h1, button, p, i, a)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (class, href, download, downloadAs)
import Ports exposing (..)
import Types exposing (..)
import Pitches exposing (..)
import Random exposing (generate)
import Array exposing (Array, fromList, toList, map)
import Random.Array exposing (shuffle)
import Midi.Types as Midi
import Midi.Generate exposing (recording)


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


generate12ToneRow : Cmd Msg
generate12ToneRow =
    Random.generate RowGenerated (Random.Array.shuffle chromaticScaleFromC4ToB)

-- dummy implementation
-- todo: convert the array to an actual MIDI sequence by mapping to the correct MidiEvents according to http://package.elm-lang.org/packages/newlandsvalley/elm-comidi/3.0.0/Midi-Types#MidiEvent
toMidi : Array Pitch -> List Midi.Byte
toMidi row =
    Midi.SingleTrack 0
        [ ( 0, Midi.NoteOn 1 40 8 )
        , ( 1500, Midi.NoteOn 1 40 8 )
        , ( 3000, Midi.NoteOn 1 40 8 )
        , ( 4500, Midi.NoteOn 1 40 8 )
        , ( 6000, Midi.NoteOn 1 40 8 )
        ]
            |> recording


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.batch [ loadPianoSamples, generate12ToneRow ] )



---- UPDATE ----


type Msg
    = NoteOn Pitch
    | NoteOff Pitch
    | RowGenerated (Array Pitch)
    | GenerateNew12ToneRow
    | TogglePlay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteOn pitch ->
            ( model, noteOn (toPitchNotation pitch) )

        NoteOff pitch ->
            ( model, noteOff (toPitchNotation pitch) )

        RowGenerated row ->
            ( Just (Stopped row), Cmd.none )

        GenerateNew12ToneRow ->
            ( model, Cmd.batch [ stopSequence (), generate12ToneRow ] )

        TogglePlay ->
            case model of
                Nothing ->
                    ( model, Cmd.none )

                Just (Stopped row) ->
                    ( Just (Playing row), startSequence (Array.map toPitchNotation row) )

                Just (Playing row) ->
                    ( Just (Stopped row), stopSequence () )



---- VIEW ----


rowView : Array Pitch -> Html Msg
rowView row =
    div [] (row |> Array.map (\p -> button [ onMouseDown (NoteOn p), onMouseUp (NoteOff p) ] [ text (toPitchNotation p) ]) |> Array.toList)


generateButton : Html Msg
generateButton =
    button [ onClick GenerateNew12ToneRow ] [ text "Generate new row" ]


rowWithControls : Array Pitch -> String -> Html Msg
rowWithControls row icon =
    div []
        [ rowView row
        , p []
            [ button [ onClick TogglePlay ] [ i [ class icon ] [] ]
            , generateButton
            -- todo: use a link button instead of just a link
            -- todo: add elm package: http://package.elm-lang.org/packages/newlandsvalley/elm-binary-base64/latest
            -- todo: use the toMidi function from above to convert the 12 tone row to a byte array and convert the byte array to a string using `encode` from http://package.elm-lang.org/packages/newlandsvalley/elm-binary-base64/latest
            -- todo: replace `text "Download"` with fontawesome download icon
            , a [ href "data:audio/midi;base64,<base64encodedbytestrig>", downloadAs "file.midi" ] [ text "Download" ]
            ]
        ]


maybeRowWithControls : Model -> Html Msg
maybeRowWithControls model =
    case model of
        Just (Stopped row) ->
            rowWithControls row "fas fa-play"

        Just (Playing row) ->
            rowWithControls row "fas fa-stop"

        Nothing ->
            div [] [ generateButton ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "luigi" ]
        , maybeRowWithControls model
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
