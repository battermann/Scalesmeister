module Model exposing (..)

import Random exposing (generate)
import Array exposing (Array)
import Random.Array exposing (shuffle)
import Audio
import Types.Pitch exposing (..)
import Types.Octave as Octave


type PlayableRow
    = Stopped (Array Pitch)
    | Playing (Array Pitch)


generate12ToneRow : Cmd Msg
generate12ToneRow =
    Random.generate RowGenerated (Random.Array.shuffle (chromaticScale Octave.four))


type alias Model =
    Maybe PlayableRow


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.batch [ Audio.loadPianoSamples, generate12ToneRow ] )


type Msg
    = NoteOn Pitch
    | NoteOff Pitch
    | RowGenerated (Array Pitch)
    | GenerateNew12ToneRow
    | TogglePlay
    | DownloadPdf
