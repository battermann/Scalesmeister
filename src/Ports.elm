port module Ports exposing (..)

{- tone.js ports -}


type alias SampleUrl =
    String


type alias ScientificPitchNotation =
    String


port loadSamples : List ( ScientificPitchNotation, SampleUrl ) -> Cmd msg


port noteOn : ScientificPitchNotation -> Cmd msg


port noteOff : ScientificPitchNotation -> Cmd msg


port startSequence : List ScientificPitchNotation -> Cmd msg


port stopSequence : () -> Cmd msg


port onCallback : (String -> msg) -> Sub msg



{- vexflow.js ports -}


type alias ElementId =
    String


type alias Notes =
    String


port renderScore : ( ElementId, Notes, Int, Int ) -> Cmd msg



{- svg2pdf.js ports -}


port downloadPdf : () -> Cmd msg
