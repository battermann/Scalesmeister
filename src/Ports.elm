port module Ports exposing (..)

import Array exposing (Array)


{- tone.js ports -}


type alias SampleUrl =
    String


type alias ScientificPitchNotation =
    String


port loadSamples : List ( ScientificPitchNotation, SampleUrl ) -> Cmd msg


port noteOn : ScientificPitchNotation -> Cmd msg


port noteOff : ScientificPitchNotation -> Cmd msg


port startSequence : Array ScientificPitchNotation -> Cmd msg


port stopSequence : () -> Cmd msg



{- vexflow.js ports -}


type alias ElementId =
    String


type alias Notes =
    String


port renderScore : ( ElementId, Notes ) -> Cmd msg



{- svg2pdf.js ports -}


port downloadPdf : () -> Cmd msg
