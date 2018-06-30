module Model exposing (..)

import Audio
import Types.Pitch exposing (..)
import Types.Octave as Octave
import Types.Line as Line exposing (..)
import Types.Scale exposing (..)
import Types.Range exposing (..)
import Types.Note exposing (..)
import Score exposing (..)
import Types.Formula as Formula exposing (..)


type PlayableLine
    = Stopped Line
    | Playing Line


type Dialog
    = SelectRoot


type alias Model =
    { range : Range
    , formula : Formula
    , root : Note
    , line : PlayableLine
    , dialog : Maybe Dialog
    }


init : ( Model, Cmd Msg )
init =
    let
        range =
            OfPitch
                { lowest = Pitch (Note C Natural) Octave.three
                , highest = Pitch (Note C Natural) Octave.six
                }

        formula =
            formula3

        root =
            Note E Flat

        initLine : Line
        initLine =
            Line.fromScaleWithinRange range (Scale root majorMinorSixthPentatonic)
                |> Line.applyFormula root formula
    in
        ( { range = range, formula = formula, root = root, line = Stopped initLine, dialog = Nothing }, Cmd.batch [ Audio.loadPianoSamples, Score.render initLine ] )


type Msg
    = NoteOn Pitch
    | NoteOff Pitch
    | TogglePlay
    | DownloadPdf
    | RootSelected Note
    | OpenSelectRootDialog
    | CloseDialog
