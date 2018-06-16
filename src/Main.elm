module Main exposing (..)

import Html exposing (Html, text, div, h1, button, p, i, a)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (class, href, download, downloadAs, id)
import Random exposing (generate)
import Array exposing (Array, fromList, toList, map)
import Random.Array exposing (shuffle)
import Audio
import Types exposing (..)
import MidiConversions
import Octave
import Score


---- MODEL ----


generate12ToneRow : Cmd Msg
generate12ToneRow =
    Random.generate RowGenerated (Random.Array.shuffle (chromaticScale Octave.middleOctave))


type alias Model =
    Maybe Row


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.batch [ Audio.loadPianoSamples, generate12ToneRow ] )



---- UPDATE ----


type Msg
    = NoteOn Pitch
    | NoteOff Pitch
    | RowGenerated (Array Pitch)
    | GenerateNew12ToneRow
    | TogglePlay
    | DownloadPdf


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteOn pitch ->
            ( model, Audio.noteOn pitch )

        NoteOff pitch ->
            ( model, Audio.noteOff pitch )

        RowGenerated row ->
            ( Just (Stopped row), Score.render row )

        GenerateNew12ToneRow ->
            ( model, Cmd.batch [ Audio.stop, generate12ToneRow ] )

        TogglePlay ->
            case model of
                Nothing ->
                    ( model, Cmd.none )

                Just (Stopped row) ->
                    ( Just (Playing row), Audio.play row )

                Just (Playing row) ->
                    ( Just (Stopped row), Audio.stop )

        DownloadPdf ->
            ( model, Score.downloadPdf )



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
        , p []
            [ a [ href (MidiConversions.createDataLink row), downloadAs "luigi.midi", class "button" ] [ i [ class "fas fa-download" ] [], text " MIDI" ]
            , button [ onClick DownloadPdf ] [ i [ class "fas fa-download" ] [], text " PDF" ]
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
        , div [ id Score.elementId ] []
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
