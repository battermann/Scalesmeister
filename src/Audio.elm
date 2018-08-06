port module Audio exposing (loadPianoSamples, play, stop, samplesLoaded)

import Types.Pitch as Pitch exposing (Pitch(..), choice, flat, sharp, natural)
import Types.PitchClass exposing (PitchClass(..), Accidental(..), Letter(..))
import Types.Octave as Octave
import Json.Encode exposing (Value)
import Types.Orchestration exposing (Orchestration(..), Beamed, Bar(..))
import Types.TimeSignature exposing (TimeSignature(..))


type alias SampleUrl =
    String


type alias ScientificPitchNotation =
    String


type alias Note =
    ( String, String )


type alias PlaybackData =
    { timeSignature : ( String, String )
    , loopEnd : String
    , noteLength : String
    , notes : List Note
    }


port loadSamples : List ( ScientificPitchNotation, SampleUrl ) -> Cmd msg


port startSequence : PlaybackData -> Cmd msg


port stopSequence : () -> Cmd msg


port samplesLoaded : (Value -> msg) -> Sub msg


toScientificPitchNotation : Pitch -> Maybe ScientificPitchNotation
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
        |> loadSamples


play : List Pitch -> Cmd msg
play pitches =
    --startSequence (List.filterMap toScientificPitchNotation pitches)
    startSequence
        { timeSignature = ( "4", "4" )
        , loopEnd = "1m"
        , noteLength = "4n"
        , notes = [ ( "0:0:0", "C4" ), ( "0:0:3", "D4" ), ( "0:1:0", "E4" ), ( "0:1:3", "F4" ) ]
        }


timeSignature : TimeSignature -> ( String, String )
timeSignature (TimeSignature numBeats beatDuration) =
    ( numBeats |> toString, beatDuration |> toString )


numberOfBars : Orchestration -> String
numberOfBars (Orchestration ts bars) =
    bars |> List.length |> toString


play2 : Orchestration -> Cmd msg
play2 (Orchestration ts bars) =
    Cmd.none


stop : Cmd msg
stop =
    stopSequence ()
