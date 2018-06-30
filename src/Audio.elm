module Audio exposing (loadPianoSamples, noteOn, noteOff, play, stop)

import Types.Pitch exposing (..)
import Types.Note exposing (..)
import Ports exposing (SampleUrl, ScientificPitchNotation)
import Types.Octave as Octave exposing (..)


toScientificPitchNotation : Pitch -> Maybe ScientificPitchNotation
toScientificPitchNotation pitch =
    case toClosestEnharmonicEquivalent pitch of
        Nothing ->
            Nothing

        Just (Pitch (Note letter accidental) octave) ->
            let
                acc =
                    case accidental of
                        Flat ->
                            Just "b"

                        Natural ->
                            Just ""

                        Sharp ->
                            Just "#"

                        _ ->
                            Nothing
            in
                acc |> Maybe.map (\accidental -> (toString letter) ++ accidental ++ (toString (Octave.number octave)))


pitchToSampleUrlMapping : Pitch -> Maybe ( ScientificPitchNotation, SampleUrl )
pitchToSampleUrlMapping (Pitch (Note letter accidental) octave) =
    let
        acc =
            case accidental of
                Natural ->
                    ""

                _ ->
                    toString accidental

        url =
            "samples/" ++ (toString letter) ++ acc ++ (toString (Octave.number octave)) ++ ".mp3"
    in
        toScientificPitchNotation (Pitch (Note letter accidental) octave)
            |> Maybe.map (\key -> ( key, url ))


loadPianoSamples : Cmd msg
loadPianoSamples =
    [ Pitch (Note C Natural) Octave.four
    , Pitch (Note D Sharp) Octave.four
    , Pitch (Note F Sharp) Octave.four
    , Pitch (Note A Natural) Octave.four
    ]
        |> List.filterMap pitchToSampleUrlMapping
        |> Ports.loadSamples


noteOn : Pitch -> Cmd msg
noteOn pitch =
    toScientificPitchNotation pitch
        |> Maybe.map Ports.noteOn
        |> Maybe.withDefault Cmd.none


noteOff : Pitch -> Cmd msg
noteOff pitch =
    toScientificPitchNotation pitch
        |> Maybe.map Ports.noteOff
        |> Maybe.withDefault Cmd.none


play : List Pitch -> Cmd msg
play pitches =
    Ports.startSequence (List.filterMap toScientificPitchNotation pitches)


stop : Cmd msg
stop =
    Ports.stopSequence ()
