module Main exposing (..)

import Html
import Array exposing (Array)
import Audio
import Types.Pitch exposing (..)
import Types.Note exposing (..)
import Types.Octave as Octave
import Score
import Model exposing (..)
import View


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


main : Program Never Model Msg
main =
    Html.program
        { view = View.view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
