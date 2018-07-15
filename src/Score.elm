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


toAbcScoreNotes : List Pitch -> String
toAbcScoreNotes pitches =
    List.Extra.greedyGroupsOf 4 pitches
        |> List.map (\groupOfFour -> groupOfFour |> List.map toAbcScoreNote |> String.join "")
        |> List.Extra.greedyGroupsOf 2
        |> List.map (\notes -> (notes |> String.join " ") ++ "|")
        |> String.join ""


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


durationToString : Duration -> String
durationToString duration =
    Note.numberOfSixteenth duration |> toString


noteOrRestToAbcNotation : Either Note Rest -> String
noteOrRestToAbcNotation noteOrRest =
    case noteOrRest of
        Left (Note pitch duration) ->
            (toAbcScoreNote pitch) ++ (duration |> durationToString)

        Right (Rest duration) ->
            "z" ++ (duration |> durationToString)


barToAbcNotation : Bar -> String
barToAbcNotation (Bar _ beamed) =
    beamed
        |> List.map (List.map noteOrRestToAbcNotation >> String.join "")
        |> String.join " "
        |> (\bar -> bar ++ "|")


orchestrationToAbcNotation : Orchestration -> String
orchestrationToAbcNotation (Orchestration timeSignature bars) =
    (mkHeader "" timeSignature |> headerToString) ++ "\n" ++ (bars |> List.map barToAbcNotation |> String.join "")
