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
        , PlaybackData
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


type alias Note =
    ( String, String )


type alias PlaybackData =
    { timeSignature : ( String, String )
    , loopEnd : String
    , noteLength : String
    , notes : List Note
    }


port loadSamples : List ( ScientificPitchNotation, SampleUrl ) -> Cmd msg


port startSequence : PlaybackData -> Cmd msg


port stopSequence : () -> Cmd msg
