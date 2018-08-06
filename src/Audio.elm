module Audio exposing (loadPianoSamples, play, stop, samplesLoaded)

import Types.Pitch as Pitch exposing (Pitch(..), choice, flat, sharp, natural)
import Types.PitchClass exposing (PitchClass(..), Accidental(..), Letter(..))
import Types.Octave as Octave
import Ports.Out
import Ports.In


toScientificPitchNotation : Pitch -> Maybe Ports.Out.ScientificPitchNotation
toScientificPitchNotation pitch =
    Pitch.enharmonicEquivalents (pitch |> Pitch.semitoneOffset)
        |> choice [ natural, sharp, flat ]
        |> Maybe.andThen
            (\(Pitch (PitchClass letter accidental) octave) ->
                let
                    maybeAcc =
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
                    maybeAcc |> Maybe.map (\acc -> toString letter ++ acc ++ toString (Octave.number octave))
            )


pitchToSampleUrlMapping : Pitch -> Maybe ( Ports.Out.ScientificPitchNotation, Ports.Out.SampleUrl )
pitchToSampleUrlMapping (Pitch (PitchClass letter accidental) octave) =
    let
        acc =
            case accidental of
                Natural ->
                    ""

                _ ->
                    toString accidental

        url =
            "samples/" ++ toString letter ++ acc ++ toString (Octave.number octave) ++ ".mp3"
    in
        toScientificPitchNotation (Pitch (PitchClass letter accidental) octave)
            |> Maybe.map (\key -> ( key, url ))


loadPianoSamples : Cmd msg
loadPianoSamples =
    [ PitchClass C Natural
    , PitchClass D Sharp
    , PitchClass F Sharp
    , PitchClass A Natural
    ]
        |> List.concatMap
            (\note ->
                [ Octave.one
                , Octave.two
                , Octave.three
                , Octave.four
                , Octave.five
                , Octave.six
                , Octave.seven
                ]
                    |> List.map (Pitch note)
            )
        |> (++) [ Pitch (PitchClass A Natural) Octave.zero, Pitch (PitchClass C Natural) Octave.eight ]
        |> List.filterMap pitchToSampleUrlMapping
        |> Ports.Out.loadSamples


play : List Pitch -> Cmd msg
play pitches =
    Ports.Out.startSequence (List.filterMap toScientificPitchNotation pitches)


stop : Cmd msg
stop =
    Ports.Out.stopSequence ()


samplesLoaded : msg -> Sub msg
samplesLoaded msg =
    Ports.In.samplesLoaded (always msg)
