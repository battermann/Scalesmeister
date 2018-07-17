module Types exposing (..)

import Types.Pitch exposing (..)
import Types.Line as Line exposing (..)
import Types.Range exposing (..)
import Types.PitchClass exposing (..)
import Score exposing (..)
import Types.Formula as Formula exposing (..)
import Types.Scale exposing (..)
import SelectList exposing (SelectList)
import Types.Interval exposing (..)
import Types.TimeSignature exposing (..)
import Types.Note as Note


type PlayingState
    = Stopped
    | Playing


type Dialog
    = SelectRoot
    | SelectScale
    | SelectFormula
    | SelectStartingNote


type alias Device =
    { width : Int
    , height : Int
    , phone : Bool
    , tablet : Bool
    , desktop : Bool
    , bigDesktop : Bool
    , portrait : Bool
    }


type alias Model =
    { range : Range
    , scales : SelectList ( String, ScaleDef )
    , formulas : SelectList Formula
    , roots : SelectList PitchClass
    , startingNote : PitchClass
    , dialog : Maybe Dialog
    , playingState : PlayingState
    , samplesLoaded : Bool
    , device : Device
    , timeSignature : TimeSignature
    , noteDuration : Note.Duration
    }


type Msg
    = TogglePlay
    | DownloadPdf
    | RootSelected PitchClass
    | StartingNoteSelected PitchClass
    | ScaleSelected ScaleDef
    | FormulaSelected Formula
    | Open Dialog
    | SamplesLoaded
    | UnknownSub String
    | WindowResize Device
    | SetTimeSignature TimeSignature
    | ToggleNoteValue
    | RangeMinStepDown
    | RangeMinStepUp
    | RangeMinSkipDown
    | RangeMinSkipUp
    | RangeMaxStepDown
    | RangeMaxStepUp
    | RangeMaxSkipDown
    | RangeMaxSkipUp
    | CloseDialog
