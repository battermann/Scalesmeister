port module Score exposing (render, downloadAsPdf, elementId)

import Types.Pitch exposing (Pitch(..))
import Types.PitchClass exposing (PitchClass(..), Accidental(..))
import Types.Octave as Octave
import Types.Orchestration exposing (Orchestration(..), Bar(..), Beamed, Clef(..))
import Types.Note as Note exposing (Note(..), Rest(..), Duration(..), Altered(..))
import Util exposing (Either(..))
import Types.TimeSignature exposing (TimeSignature(..), beatDurationToInt, numberOfBeatsToInt)
import List.Extra


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


clefToAbcNotation : Clef -> String
clefToAbcNotation c =
    case c of
        Treble ->
            "[K: treble]"

        Bass ->
            "[K: bass]"


headerToString : Header -> String
headerToString (Header (ReferenceNumber x) (Title title) (Meter beatsPerBar beatUnit)) =
    [ "X: " ++ toString x

    --, "%%stretchlast 1"
    , "T: " ++ title
    , "M: " ++ toString beatsPerBar ++ "/" ++ toString beatUnit
    , "L: 1/16"
    , "K: C"
    ]
        |> String.join "\n"


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

        Note.Quarter Dotted ->
            note ++ "6"

        Note.Quarter _ ->
            note ++ "4"

        Note.Eighth Dotted ->
            note ++ "3"

        Note.Eighth _ ->
            note ++ "2"

        Note.Sixteenth ->
            note ++ "1"


noteOrRestToAbcNotation : Either Note Rest -> String
noteOrRestToAbcNotation noteOrRest =
    case noteOrRest of
        Left (Note pitch duration) ->
            toAbcScoreNote pitch |> addAbcDuration duration

        Right (Rest duration) ->
            "z" |> addAbcDuration duration


getDuration : Either Note Rest -> Duration
getDuration noteOrRest =
    case noteOrRest of
        Left (Note _ duration) ->
            duration

        Right (Rest duration) ->
            duration


isTriplet : Either Note Rest -> Bool
isTriplet noteOrRest =
    case getDuration noteOrRest of
        Note.Quarter Note.Triplet ->
            True

        Note.Eighth Note.Triplet ->
            True

        _ ->
            False


beamedToAbcNotation : Beamed -> String
beamedToAbcNotation list =
    let
        abc =
            list |> List.map noteOrRestToAbcNotation >> String.join ""
    in
        if list |> List.all isTriplet then
            "(3:2:" ++ (list |> List.length |> toString) ++ abc
        else
            abc


barToAbcNotation : Bar -> String
barToAbcNotation (Bar clef beamed) =
    beamed
        |> List.map beamedToAbcNotation
        |> String.join " "
        |> (\bar -> bar ++ "|")
        |> (++) (clef |> Maybe.map clefToAbcNotation |> Maybe.withDefault "")


orchestrationToAbcNotation : Orchestration -> String
orchestrationToAbcNotation (Orchestration timeSignature bars) =
    (mkHeader "" timeSignature |> headerToString) ++ "\n" ++ (bars |> List.Extra.greedyGroupsOf 3 |> List.map (List.map barToAbcNotation >> String.join "") |> String.join "\n")
