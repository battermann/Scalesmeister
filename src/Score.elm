module Score exposing (render, downloadPdf, elementId)

import Array exposing (Array)
import Pitch exposing (..)
import Ports


toEasyScorePitchNotation : Pitch -> PitchNotation
toEasyScorePitchNotation pitch =
    (toPitchNotationCustomNatural "n" pitch) ++ "/q"


toEasyScoreNotation : Array Pitch -> String
toEasyScoreNotation row =
    row |> Array.map toEasyScorePitchNotation |> Array.toList |> String.join ", "


elementId : Ports.ElementId
elementId =
    "score"


render : Array Pitch -> Cmd msg
render pitches =
    Ports.renderScore ( elementId, toEasyScoreNotation pitches )


downloadPdf : Cmd msg
downloadPdf =
    Ports.downloadPdf ()
