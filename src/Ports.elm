port module Ports exposing (..)

import Types exposing (..)
import Array exposing (Array)


type alias SampleUrl =
    String


port loadSamples : List ( PitchNotation, SampleUrl ) -> Cmd msg


port noteOn : PitchNotation -> Cmd msg


port noteOff : PitchNotation -> Cmd msg


port startSequence : Array PitchNotation -> Cmd msg


port stopSequence : () -> Cmd msg


type alias ElementId =
    String


type alias ScoreLine =
    String


port renderScore : ( ElementId, ScoreLine ) -> Cmd msg


port downloadPdf : () -> Cmd msg
