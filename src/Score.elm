module Score exposing (render, downloadPdf, elementId)

import Array exposing (Array)
import Types.Tonal exposing (..)
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
                    "n"

                Sharp ->
                    "#"

                DoubleSharp ->
                    "##"
    in
        (toString letter) ++ acc ++ (toString (Octave.number octave)) ++ "/q"


toEasyScoreNotation : Array Pitch -> String
toEasyScoreNotation =
    Array.map toEasyScoreNote >> Array.toList >> String.join ", "


elementId : Ports.ElementId
elementId =
    "score"


render : Array Pitch -> Cmd msg
render pitches =
    Ports.renderScore ( elementId, toEasyScoreNotation pitches )


downloadPdf : Cmd msg
downloadPdf =
    Ports.downloadPdf ()
