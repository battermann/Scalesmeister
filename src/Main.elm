module Main exposing (..)

import Html exposing (Html, text, div, h1, button, p, i, a)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (class, href, download, downloadAs)
import Ports exposing (..)
import Types exposing (..)
import Random exposing (generate)
import Array exposing (Array, fromList, toList, map)
import Random.Array exposing (shuffle)
import Midi.Types as Midi
import Midi.Generate exposing (recording)
import BinaryBase64 exposing (encode)


---- MODEL ----


toPitchNotation : Pitch -> PitchNotation
toPitchNotation (Pitch note accidental octave) =
    let
        acc =
            case accidental of
                Just Sharp ->
                    "#"

                Just Flat ->
                    "b"

                Nothing ->
                    ""
    in
        (toString note) ++ acc ++ (toString octave)


pitchToSampleUrlMapping : Pitch -> ( PitchNotation, SampleUrl )
pitchToSampleUrlMapping (Pitch note accidental octave) =
    let
        acc =
            Maybe.map toString accidental
                |> Maybe.withDefault ""

        url =
            "samples/" ++ (toString note) ++ acc ++ (toString octave) ++ ".mp3"
    in
        ( toPitchNotation (Pitch note accidental octave), url )


loadPianoSamples : Cmd msg
loadPianoSamples =
    [ Pitch C Nothing 4
    , Pitch D (Just Sharp) 4
    , Pitch F (Just Sharp) 4
    , Pitch A Nothing 4
    ]
        |> List.map pitchToSampleUrlMapping
        |> loadSamples


generate12ToneRow : Cmd Msg
generate12ToneRow =
    Random.generate RowGenerated (Random.Array.shuffle (chromaticScale 4))

-- dummy implementation
-- todo: Create a valid midi file, currently the generated midi file doesn't work, check specification and correct serialization
-- todo: convert the array to an actual MIDI sequence by mapping to the correct MidiEvents according to http://package.elm-lang.org/packages/newlandsvalley/elm-comidi/3.0.0/Midi-Types#MidiEvent
toMidi : Array Pitch -> List Midi.Byte
toMidi row =
    Midi.SingleTrack 0
        [ ( 0, Midi.ProgramChange 1 1 )
        , ( 0, Midi.NoteOn 0 60 64 )
        , ( 15, Midi.NoteOff 0 60 0 )
        , ( 15, Midi.NoteOn 0 61 64 )
        , ( 30, Midi.NoteOff 0 61 0 )
        , ( 30, Midi.NoteOn 0 62 64 )
        , ( 45, Midi.NoteOff 0 62 0 )
        , ( 45, Midi.NoteOn 0 63 64 )
        , ( 60, Midi.NoteOff 0 63 0 )
        , ( 60, Midi.NoteOn 0 64 64 )
        , ( 75, Midi.NoteOff 0 64 0 )
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
            -- todo: adjust layout / element size
            , a [ href ("data:audio/midi;base64," ++ (encode (toMidi row))), downloadAs "luigi.midi", class "button" ] [ i [ class "fas fa-download" ] [] ]
            -- the hardcoded version below seems to work
            --, a [ href "data:audio/midi;base64,TVRoZAAAAAYAAAABAIBNVHJrAAAAhAGRPH88gTx/AJE+fzyBPn8AkUB/PIFAfwCRQn88gUJ/AJFDfzyBQ38AkUV/PIFFfwCRR388gUd/AJFIfzyBSH8AkUh/PIFIfwCRR388gUd/AJFFfzyBRX8AkUN/PIFDfwCRQn88gUJ/AJFAfzyBQH8AkT5/PIE+fwCRPH88gTx/AP8vAA==", downloadAs "luigi.midi", class "button" ] [ i [ class "fas fa-download" ] [] ]
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
