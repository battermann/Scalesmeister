port module Ports.Out
    exposing
        ( elementId
        , renderScore
        , downloadPdf
        , loadSamples
        , startSequence
        , stopSequence
        , ScientificPitchNotation
        , SampleUrl
        , ElementId
        )

--- SCORE


type alias ElementId =
    String


type alias AbcNotation =
    String


elementId : ElementId
elementId =
    "score"


port renderScore : ( ElementId, AbcNotation ) -> Cmd msg


port downloadPdf : () -> Cmd msg



---- AUDIO


type alias SampleUrl =
    String


type alias ScientificPitchNotation =
    String


port loadSamples : List ( ScientificPitchNotation, SampleUrl ) -> Cmd msg


port startSequence : List ScientificPitchNotation -> Cmd msg


port stopSequence : () -> Cmd msg
