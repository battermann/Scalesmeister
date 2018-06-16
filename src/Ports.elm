port module Ports exposing (..)

import Types exposing (..)
import Array exposing (Array)


port noteOn : PitchNotation -> Cmd msg


port noteOff : PitchNotation -> Cmd msg


port loadSamples : List ( PitchNotation, SampleUrl ) -> Cmd msg


port startSequence : Array PitchNotation -> Cmd msg


port stopSequence : () -> Cmd msg


type alias ElementId =
    String


scoreElementId : ElementId
scoreElementId =
    "score"


type alias ScoreLine =
    String


port renderScore : ( ElementId, ScoreLine ) -> Cmd msg


port downloadPdf : () -> Cmd msg
