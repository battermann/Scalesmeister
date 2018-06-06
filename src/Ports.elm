port module Ports exposing (..)

import Types exposing (..)


port noteOn : PitchNotation -> Cmd msg


port noteOff : PitchNotation -> Cmd msg


port loadSamples : List ( PitchNotation, SampleUrl ) -> Cmd msg


port startSequence : List PitchNotation -> Cmd msg


port stopSequence : () -> Cmd msg
