module Update exposing (..)

import Audio
import Score
import Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteOn pitch ->
            ( model, Audio.noteOn pitch )

        NoteOff pitch ->
            ( model, Audio.noteOff pitch )

        TogglePlay ->
            case model of
                Stopped line ->
                    ( Playing line, Audio.play line )

                Playing line ->
                    ( Stopped line, Audio.stop )

        DownloadPdf ->
            ( model, Score.downloadPdf )
