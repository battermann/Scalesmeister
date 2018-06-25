module Model exposing (..)

import Audio
import Types.Pitch exposing (..)
import Types.Octave as Octave
import Types.Line as Line exposing (..)
import Types.Scale exposing (..)
import Types.Range exposing (..)
import Types.Note exposing (..)
import Score exposing (..)
import Types.Formula exposing (..)


type PlayableLine
    = Stopped Line
    | Playing Line


type alias Model =
    PlayableLine


range : Range
range =
    OfPitch
        { lowest = Pitch (Note B Flat) Octave.two
        , highest = Pitch (Note C Natural) Octave.six
        }


formula : Formula
formula =
    [ -2, -1, 2, -1 ]


initLine : Line
initLine =
    Line.fromScaleWithinRange range (Scale (Note C Natural) minorPentatonic)
        |> Line.applyFormula (Note C Natural) formula


init : ( Model, Cmd Msg )
init =
    ( Stopped initLine, Cmd.batch [ Audio.loadPianoSamples, Score.render initLine ] )


type Msg
    = NoteOn Pitch
    | NoteOff Pitch
    | TogglePlay
    | DownloadPdf
