port module Ports exposing (..)

import Types exposing (..)
import Array exposing (Array)


port noteOn : PitchNotation -> Cmd msg


port noteOff : PitchNotation -> Cmd msg


port loadSamples : List ( PitchNotation, SampleUrl ) -> Cmd msg


port startSequence : Array PitchNotation -> Cmd msg


port stopSequence : () -> Cmd msg
