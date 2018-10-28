module Types exposing (Device, Dialog(..), Model, Msg(..), PlayingState(..))

import Libs.SelectList exposing (SelectList)
import MusicTheory.PitchClass exposing (PitchClass)
import MusicTheory.ScaleClass exposing (ScaleClass)
import Types.Formula exposing (Formula)
import Types.Note as Note
import Types.Range exposing (Range)
import Types.Switch exposing (Switch)
import Types.TimeSignature exposing (TimeSignature)


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
    , scales : SelectList ( String, ScaleClass )
    , formulas : SelectList Formula
    , roots : SelectList PitchClass
    , startingNote : PitchClass
    , dialog : Maybe Dialog
    , playingState : PlayingState
    , samplesLoaded : Bool
    , device : Device
    , timeSignature : TimeSignature
    , noteDuration : Note.Duration
    , clickTrack : Switch
    , tempo : Float
    }


type Msg
    = TogglePlay
    | DownloadPdf
    | RootSelected PitchClass
    | StartingNoteSelected PitchClass
    | ScaleSelected String
    | FormulaSelected Formula
    | Open Dialog
    | SamplesLoaded
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
    | ToggleClick
    | UpdateTempo Float
