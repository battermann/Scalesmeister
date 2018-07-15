port module Audio exposing (loadPianoSamples, play, stop, samplesLoaded)

import Types.Pitch as Pitch exposing (..)
import Types.PitchClass exposing (..)
import Types.Octave as Octave exposing (..)
import List.Extra
import Json.Encode exposing (Value)
import Json.Decode as Decode


type alias SampleUrl =
    String


type alias ScientificPitchNotation =
    String


port loadSamples : List ( ScientificPitchNotation, SampleUrl ) -> Cmd msg


port startSequence : List ScientificPitchNotation -> Cmd msg


port stopSequence : () -> Cmd msg


port samplesLoaded : (Value -> msg) -> Sub msg


toScientificPitchNotation : Pitch -> Maybe ScientificPitchNotation
toScientificPitchNotation pitch =
    case Pitch.enharmonicEquivalents (pitch |> Pitch.semitoneOffset) |> choice [ natural, sharp, flat ] of
        Nothing ->
            Nothing

        Just (Pitch (PitchClass letter accidental) octave) ->
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
pitchToSampleUrlMapping (Pitch (PitchClass letter accidental) octave) =
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
        |> ((++) [ Pitch (PitchClass A Natural) Octave.zero, Pitch (PitchClass C Natural) Octave.eight ])
        |> List.filterMap pitchToSampleUrlMapping
        |> loadSamples


play : List Pitch -> Cmd msg
play pitches =
    startSequence (List.filterMap toScientificPitchNotation pitches)


stop : Cmd msg
stop =
    stopSequence ()
