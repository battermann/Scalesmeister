module Audio exposing (loadPianoSamples, noteOn, noteOff, play, stop)

import Types.Pitch exposing (..)
import Types.Note exposing (..)
import Ports exposing (SampleUrl, ScientificPitchNotation)
import Types.Octave as Octave exposing (..)


toScientificPitchNotation : Pitch -> ScientificPitchNotation
toScientificPitchNotation (Pitch (Note letter accidental) octave) =
    let
        acc =
            case accidental of
                Flat ->
                    "b"

                Natural ->
                    ""

                Sharp ->
                    "#"

                _ ->
                    Debug.crash "double flats and sharps are not defined in scientific pitch notation, notes have to be replaced with a proper equivalent note with the same pitch"
    in
        (toString letter) ++ acc ++ (toString (number octave))


pitchToSampleUrlMapping : Pitch -> ( ScientificPitchNotation, SampleUrl )
pitchToSampleUrlMapping (Pitch (Note letter accidental) octave) =
    let
        acc =
            case accidental of
                Natural ->
                    ""

                _ ->
                    toString accidental

        url =
            "samples/" ++ (toString letter) ++ acc ++ (toString (number octave)) ++ ".mp3"
    in
        ( toScientificPitchNotation (Pitch (Note letter accidental) octave), url )


loadPianoSamples : Cmd msg
loadPianoSamples =
    [ Pitch (Note C Natural) Octave.four
    , Pitch (Note D Sharp) Octave.four
    , Pitch (Note F Sharp) Octave.four
    , Pitch (Note A Natural) Octave.four
    ]
        |> List.map pitchToSampleUrlMapping
        |> Ports.loadSamples


noteOn : Pitch -> Cmd msg
noteOn pitch =
    Ports.noteOn (toScientificPitchNotation pitch)


noteOff : Pitch -> Cmd msg
noteOff pitch =
    Ports.noteOff (toScientificPitchNotation pitch)


play : List Pitch -> Cmd msg
play pitches =
    Ports.startSequence (List.map toScientificPitchNotation pitches)


stop : Cmd msg
stop =
    Ports.stopSequence ()
