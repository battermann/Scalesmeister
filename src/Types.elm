module Types exposing (Device, Dialog(..), Error(..), Model, Msg(..), PlayingState(..), Settings, pitchClasses, validStartingNote)

import Browser
import Browser.Navigation as Nav
import Libs.SelectList as SelectList exposing (SelectList)
import MusicTheory.PitchClass exposing (PitchClass)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass exposing (ScaleClass)
import Types.Formula exposing (Formula)
import Types.Note as Note
import Types.Range exposing (Range)
import Types.Switch exposing (Switch)
import Types.TimeSignature exposing (TimeSignature)
import Url


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


type Error
    = CantCreateLine


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
    , key : Nav.Key
    , error : Maybe Error
    , randomUrl : String
    }


type alias Settings =
    { root : PitchClass
    , scale : ( String, ScaleClass )
    , formula : Formula
    , startingNote : PitchClass
    }


pitchClasses : Model -> List PitchClass
pitchClasses model =
    model.scales |> SelectList.selected |> Tuple.second |> Scale.scale (SelectList.selected model.roots) |> Scale.toList


validStartingNote : PitchClass -> ScaleClass -> PitchClass -> Bool
validStartingNote root scaleClass pitchClass =
    Scale.scale root scaleClass |> Scale.toList |> List.member pitchClass


type Msg
    = CloseDialog
    | DownloadPdf
    | FormulaPresetSelected Formula
    | FormulaInput String
    | LinkClicked Browser.UrlRequest
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
    | SamplesLoaded
    | SetTimeSignature TimeSignature
    | ToggleAdvancedControls
    | ToggleClick
    | ToggleNoteValue
    | TogglePlay
    | UpdateTempo Float
    | UrlChanged Url.Url
    | WindowResize Device
    | RandomUrlCreated String
