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
    , formula : Formula
    , formulaInput : String
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
    , advancedControls : Bool
    }


type Msg
    = CloseDialog
    | DownloadPdf
    | FormulaSelected Formula
    | FormulaPresetSelected Formula
    | FormulaInput String
    | NoOp
    | Open Dialog
    | RangeMaxSkipDown
    | RangeMaxSkipUp
    | RangeMaxStepDown
    | RangeMaxStepUp
    | RangeMinSkipDown
    | RangeMinSkipUp
    | RangeMinStepDown
    | RangeMinStepUp
    | RootSelected PitchClass
    | SamplesLoaded
    | ScaleSelected String
    | SetTimeSignature TimeSignature
    | StartingNoteSelected PitchClass
    | ToggleAdvancedControls
    | ToggleClick
    | ToggleNoteValue
    | TogglePlay
    | UpdateTempo Float
    | WindowResize Device
