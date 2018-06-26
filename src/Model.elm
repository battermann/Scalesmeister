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


type alias Model =
    PlayableLine


range : Range
range =
    OfPitch
        { lowest = Pitch (Note C Natural) Octave.four
        , highest = Pitch (Note C Natural) Octave.six
        }


formula : Formula
formula =
    [ 1, 1, 1, 1 ]


initLine : Line
initLine =
    Line.fromScaleWithinRange range (Scale (Note A Flat) minorSevenDiminishedFifthPentatonic)
        |> Line.applyFormula (Note A Flat) formula


init : ( Model, Cmd Msg )
init =
    ( Stopped initLine, Cmd.batch [ Audio.loadPianoSamples, Score.render initLine ] )


type Msg
    = NoteOn Pitch
    | NoteOff Pitch
    | TogglePlay
    | DownloadPdf
