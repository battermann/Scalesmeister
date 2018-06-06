port module Ports exposing (..)

import Types exposing (..)

port noteOn : PitchNotation -> Cmd msg

port noteOff : PitchNotation -> Cmd msg

port loadSamples : List ( PitchNotation, SampleUrl ) -> Cmd msg
