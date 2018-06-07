module Main exposing (..)

import Html exposing (Html, text, div, h1, button, p, i)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (class)
import Ports exposing (..)
import Types exposing (..)
import Pitches exposing (..)
import Random exposing (generate)
import Array exposing (Array, fromList, toList, map)
import Random.Array exposing (shuffle)


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


init : ( Model, Cmd Msg )
init =
    let
        cmd =
            Cmd.batch [ loadPianoSamples, generate12ToneRow ]
    in
        ( Nothing, cmd )



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
            ( Nothing, Cmd.batch [ stopSequence (), generate12ToneRow ] )

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
