module Audio exposing (loadPianoSamples, noteOn, noteOff, play, stop)

import Array exposing (Array)
import Types exposing (..)
import Ports exposing (SampleUrl)
import Octave exposing (..)


pitchToSampleUrlMapping : Pitch -> ( PitchNotation, SampleUrl )
pitchToSampleUrlMapping (Pitch note accidental octave) =
    let
        acc =
            Maybe.map toString accidental
                |> Maybe.withDefault ""

        url =
            "samples/" ++ (toString note) ++ acc ++ (toString (number octave)) ++ ".mp3"
    in
        ( toPitchNotation (Pitch note accidental octave), url )


loadPianoSamples : Cmd msg
loadPianoSamples =
    [ Pitch C Nothing middleOctave
    , Pitch D (Just Sharp) middleOctave
    , Pitch F (Just Sharp) middleOctave
    , Pitch A Nothing middleOctave
    ]
        |> List.map pitchToSampleUrlMapping
        |> Ports.loadSamples


noteOn : Pitch -> Cmd msg
noteOn pitch =
    Ports.noteOn (toPitchNotation pitch)


noteOff : Pitch -> Cmd msg
noteOff pitch =
    Ports.noteOff (toPitchNotation pitch)


play : Array Pitch -> Cmd msg
play pitches =
    Ports.startSequence (Array.map toPitchNotation pitches)


stop : Cmd msg
stop =
    Ports.stopSequence ()
