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
import BinaryBase64 exposing (encode, decode)


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
    [ Pitch C Nothing middleOctave
    , Pitch D (Just Sharp) middleOctave
    , Pitch F (Just Sharp) middleOctave
    , Pitch A Nothing middleOctave
    ]
        |> List.map pitchToSampleUrlMapping
        |> loadSamples


generate12ToneRow : Cmd Msg
generate12ToneRow =
    Random.generate RowGenerated (Random.Array.shuffle (chromaticScale 4))


flatten : List (List a) -> List a
flatten list =
    List.foldr (++) [] list


zipWithIndex : List a -> List ( a, Int )
zipWithIndex list =
    List.range 0 (List.length list) |> List.map2 (,) list


toMidi : Array Pitch -> List Midi.Byte
toMidi row =
    let
        track =
            row
                |> Array.toList
                |> List.map toMidiNumber
                |> zipWithIndex
                |> List.map
                    (\( midiNumber, i ) ->
                        [ ( 0, Midi.NoteOn 0 midiNumber 64 )
                        , ( 2, Midi.NoteOff 0 midiNumber 0 )
                        ]
                    )
                |> flatten

        _ =
            Debug.log "" track
    in
        track
            |> Midi.SingleTrack 4
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
            , a [ href ("data:audio/midi;base64," ++ (encode (toMidi row))), downloadAs "luigi.midi", class "button" ] [ i [ class "fas fa-download" ] [] ]
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
