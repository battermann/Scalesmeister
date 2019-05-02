module Score exposing (downloadAsPdf, elementId, render)

import List.Extra
import MusicTheory.Letter as Letter
import MusicTheory.Octave as Octave
import MusicTheory.Pitch exposing (Pitch)
import MusicTheory.Pitch.Spelling as Spelling
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import Ports.Out
import Types.Note as Note exposing (Altered(..), Duration(..), Note(..), Rest(..))
import Types.Orchestration exposing (Bar(..), Beamed, Clef(..), DisplayNote(..), Orchestration(..))
import Types.TimeSignature as TimeSignature exposing (TimeSignature(..))
import Util exposing (Either(..))


elementId : Ports.Out.ElementId
elementId =
    "score"


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


toAbcScoreNote : Bool -> Pitch -> String
toAbcScoreNote showAccidental pitch =
    case pitch |> Spelling.simple of
        Ok { letter, accidental, octave } ->
            let
                acc =
                    if showAccidental then
                        case accidental of
                            Flat ->
                                "_"

                            Natural ->
                                "="

                            Sharp ->
                                "^"

                    else
                        ""
            in
            if Octave.number octave < 5 then
                acc ++ (letter |> Letter.toString) ++ (List.repeat (4 - Octave.number octave) ',' |> String.fromList)

            else
                acc ++ (letter |> Letter.toString |> String.toLower) ++ (List.repeat (Octave.number octave - 5) '\'' |> String.fromList)

        Err _ ->
            ""


clefToAbcNotation : Clef -> String
clefToAbcNotation c =
    case c of
        Treble ->
            "[K: treble]"

        Bass ->
            "[K: bass]"


headerToString : Header -> String
headerToString (Header (ReferenceNumber x) (Title title) (Meter beatsPerBar beatUnit)) =
    [ "X: " ++ String.fromInt x

    --, "%%stretchlast 1"
    , "T: " ++ title
    , "M: " ++ String.fromInt beatsPerBar ++ "/" ++ String.fromInt beatUnit
    , "L: 1/16"
    , "K: C"
    ]
        |> String.join "\n"


render : Orchestration -> Cmd msg
render orchestration =
    Ports.Out.renderScore ( Ports.Out.elementId, orchestration |> orchestrationToAbcNotation )


timeSignature : TimeSignature -> Meter
timeSignature (TimeSignature numBeats beatDuration) =
    Meter (numBeats |> TimeSignature.numberOfBeatsToInt) (beatDuration |> TimeSignature.beatDurationToInt)


downloadAsPdf : Cmd msg
downloadAsPdf =
    Ports.Out.downloadPdf ()


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


noteOrRestToAbcNotation : Either DisplayNote Rest -> String
noteOrRestToAbcNotation noteOrRest =
    case noteOrRest of
        Left (DisplayNote showAccidental (Note pitch duration)) ->
            toAbcScoreNote showAccidental pitch |> addAbcDuration duration

        Right (Rest duration) ->
            "z" |> addAbcDuration duration


getDuration : Either DisplayNote Rest -> Duration
getDuration noteOrRest =
    case noteOrRest of
        Left (DisplayNote _ (Note _ duration)) ->
            duration

        Right (Rest duration) ->
            duration


isTriplet : Either DisplayNote Rest -> Bool
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
        "(3:2:" ++ (list |> List.length |> String.fromInt) ++ abc

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
orchestrationToAbcNotation (Orchestration ts bars) =
    (mkHeader "" ts |> headerToString) ++ "\n" ++ (bars |> List.Extra.greedyGroupsOf 3 |> List.map (List.map barToAbcNotation >> String.join "") |> String.join "\n")
