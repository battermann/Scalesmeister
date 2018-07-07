module Score exposing (render, downloadPdf, elementId)

import Types.Pitch exposing (..)
import Types.Note exposing (..)
import Types.Octave as Octave
import List.Extra
import Ports


type Header
    = Header ReferenceNumber Title Meter BasicNoteLength


type Title
    = Title String


type Meter
    = Meter Int Int


type ReferenceNumber
    = ReferenceNumber Int


type BasicNoteLength
    = BasicNoteLength Int Int


mkHeader : String -> Header
mkHeader title =
    Header (ReferenceNumber 1) (Title title) (Meter 4 4) (BasicNoteLength 1 8)



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
toAbcScoreNote (Pitch (Note letter accidental) octave) =
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
headerToString (Header (ReferenceNumber x) (Title title) (Meter beatsPerBar beatUnit) (BasicNoteLength numerator denominator)) =
    "X: " ++ (toString x) ++ "\n%%stretchlast 1\n" ++ "T: " ++ title ++ "\n" ++ "M: " ++ (toString beatsPerBar) ++ "/" ++ (toString beatUnit) ++ "\n" ++ "L: " ++ (toString numerator) ++ "/" ++ (toString denominator) ++ "\n" ++ "K: C"


toAbcNotation : List Pitch -> String
toAbcNotation pitches =
    (mkHeader "" |> headerToString) ++ "\n" ++ (toAbcScoreNotes pitches)



--(mkHeader "line" |> headerToString) ++ "\n" ++ """"8va"C D E F G A"""


elementId : Ports.ElementId
elementId =
    "score"


render : List Pitch -> Cmd msg
render pitches =
    Ports.renderScore ( elementId, toAbcNotation pitches )


downloadPdf : Cmd msg
downloadPdf =
    Ports.downloadPdf ()
