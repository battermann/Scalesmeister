module Score exposing (render, downloadPdf, elementId)

import Types.Pitch exposing (..)
import Types.Note exposing (..)
import Types.Octave as Octave
import Ports


toEasyScoreNote : Pitch -> String
toEasyScoreNote (Pitch (Note letter accidental) octave) =
    let
        acc =
            case accidental of
                DoubleFlat ->
                    "bb"

                Flat ->
                    "b"

                Natural ->
                    ""

                Sharp ->
                    "#"

                DoubleSharp ->
                    "##"
    in
        (toString letter) ++ acc ++ (toString (Octave.number octave)) ++ "/q"


toEasyScoreNotation : List Pitch -> String
toEasyScoreNotation =
    List.map toEasyScoreNote >> String.join ", "


elementId : Ports.ElementId
elementId =
    "score"


render : List Pitch -> Cmd msg
render pitches =
    Ports.renderScore ( elementId, toEasyScoreNotation pitches, pitches |> List.length, 4 )


downloadPdf : Cmd msg
downloadPdf =
    Ports.downloadPdf ()
