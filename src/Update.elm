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
            case model.line of
                Stopped line ->
                    ( { model | line = Playing line }, Audio.play line )

                Playing line ->
                    ( { model | line = Stopped line }, Audio.stop )

        DownloadPdf ->
            ( model, Score.downloadPdf )

        OpenSelectRootDialog ->
            ( { model | dialog = Just SelectRoot }, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, Cmd.none )

        RootSelected note ->
            ( { model | root = note }, Cmd.none )
