port module Score exposing (render, downloadAsPdf, elementId)

import Types.Pitch exposing (..)
import Types.PitchClass exposing (..)
import Types.Octave as Octave
import List.Extra
import Types.Orchestration exposing (..)
import Types.Note exposing (..)
import Types.Orchestration as Orchestration
import Helpers exposing (Either(..))
import Types.TimeSignature as TimeSignature exposing (..)
import Types.Note as Note


type alias ElementId =
    String


type alias AbcNotation =
    String


elementId : ElementId
elementId =
    "score"


port renderScore : ( ElementId, AbcNotation ) -> Cmd msg


port downloadPdf : () -> Cmd msg


type Header
    = Header ReferenceNumber Title Meter


type Title
    = Title String


type Meter
    = Meter Int Int


type ReferenceNumber
    = ReferenceNumber Int


mkHeader : String -> TimeSignature -> Header
mkHeader title ts =
    Header (ReferenceNumber 1) (Title title) (timeSignature ts)



{-

   octave notation

   0 C,,,,
   1 C,,,
   2 C,,
   3 C,
   4 C
   5 c
   6 c'
   7 c''
   8 c'''

-}


toAbcScoreNote : Pitch -> String
toAbcScoreNote (Pitch (PitchClass letter accidental) octave) =
    let
        acc =
            case accidental of
                DoubleFlat ->
                    "__"

                Flat ->
                    "_"

                Natural ->
                    ""

                Sharp ->
                    "^"

                DoubleSharp ->
                    "^"
    in
        if Octave.number octave < 5 then
            acc ++ (letter |> toString) ++ (List.repeat (4 - Octave.number octave) ',' |> String.fromList)
        else
            acc ++ (letter |> toString |> String.toLower) ++ (List.repeat (Octave.number octave - 5) '\'' |> String.fromList)


headerToString : Header -> String
headerToString (Header (ReferenceNumber x) (Title title) (Meter beatsPerBar beatUnit)) =
    "X: " ++ (toString x) ++ "\n%%stretchlast 1\n" ++ "T: " ++ title ++ "\n" ++ "M: " ++ (toString beatsPerBar) ++ "/" ++ (toString beatUnit) ++ "\n" ++ "L: 1/16" ++ "\n" ++ "K: C"


render : Orchestration -> Cmd msg
render orchestration =
    renderScore ( elementId, orchestration |> orchestrationToAbcNotation )


timeSignature : TimeSignature -> Meter
timeSignature (TimeSignature numBeats beatDuration) =
    Meter (numBeats |> numberOfBeatsToInt) (beatDuration |> beatDurationToInt)


downloadAsPdf : Cmd msg
downloadAsPdf =
    downloadPdf ()


addAbcDuration : Duration -> String -> String
addAbcDuration duration note =
    case duration of
        Note.Whole ->
            note ++ "16"

        Note.Half ->
            note ++ "8"

        Note.Quarter ->
            note ++ "4"

        Note.Eighth None ->
            note ++ "2"

        Note.Eighth Triplet ->
            "(3" ++ note ++ "2"

        Note.Sixteenth ->
            note ++ "1"


noteOrRestToAbcNotation : Either Note Rest -> String
noteOrRestToAbcNotation noteOrRest =
    case noteOrRest of
        Left (Note pitch duration) ->
            toAbcScoreNote pitch |> addAbcDuration duration

        Right (Rest duration) ->
            "z" |> addAbcDuration duration


barToAbcNotation : Bar -> String
barToAbcNotation (Bar _ beamed) =
    beamed
        |> List.map (List.map noteOrRestToAbcNotation >> String.join "")
        |> String.join " "
        |> (\bar -> bar ++ "|")


orchestrationToAbcNotation : Orchestration -> String
orchestrationToAbcNotation (Orchestration timeSignature bars) =
    (mkHeader "" timeSignature |> headerToString) ++ "\n" ++ (bars |> List.map barToAbcNotation |> String.join "")
