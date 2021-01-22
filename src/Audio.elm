module Audio exposing (loadPianoSamples, muteClick, play, samplesLoaded, setTempo, stop, unMuteClick)

import Libs.Ratio as Ratio
import List.Extra
import MusicTheory.Letter as Letter exposing (Letter(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.Pitch.Spelling as Spelling
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import Ports.In
import Ports.Out
import Types.Note as Note exposing (Altered(..), Duration(..))
import Types.Switch as Switch exposing (Switch)
import Types.TimeSignature as TimeSignature exposing (TimeSignature(..))


toScientificPitchNotation : Pitch -> Maybe Ports.Out.ScientificPitchNotation
toScientificPitchNotation pitch =
    let
        accToString acc =
            case acc of
                Flat ->
                    "b"

                Natural ->
                    ""

                Sharp ->
                    "#"
    in
    pitch
        |> Spelling.simple
        |> Result.map
            (\spelling ->
                Letter.toString spelling.letter ++ accToString spelling.accidental ++ String.fromInt (Octave.number spelling.octave)
            )
        |> Result.toMaybe


pitchToSampleUrlMapping : Pitch -> Maybe ( Ports.Out.ScientificPitchNotation, Ports.Out.SampleUrl )
pitchToSampleUrlMapping pitch =
    let
        accidentalToString accidental =
            case accidental of
                Flat ->
                    "Flat"

                Natural ->
                    ""

                Sharp ->
                    "Sharp"

        url spelling =
            "samples/" ++ Letter.toString spelling.letter ++ accidentalToString spelling.accidental ++ String.fromInt (Octave.number spelling.octave) ++ ".mp3"
    in
    Maybe.map2 Tuple.pair
        (toScientificPitchNotation pitch)
        (pitch |> Spelling.simple |> Result.map url |> Result.toMaybe)


loadPianoSamples : Cmd msg
loadPianoSamples =
    [ ( C, Pitch.natural )
    , ( D, Pitch.sharp )
    , ( F, Pitch.sharp )
    , ( A, Pitch.natural )
    ]
        |> List.concatMap
            (\( letter, accidental ) ->
                [ Octave.one
                , Octave.two
                , Octave.three
                , Octave.four
                , Octave.five
                , Octave.six
                , Octave.seven
                ]
                    |> List.map (\o -> Pitch.pitch letter accidental o)
            )
        |> List.append [ Pitch.pitch A Pitch.natural Octave.zero, Pitch.pitch C Pitch.natural Octave.eight ]
        |> List.filterMap pitchToSampleUrlMapping
        |> Ports.Out.loadSamples


play : Switch -> TimeSignature -> Duration -> List Pitch -> Cmd msg
play clickTrackSwitch ts duration line =
    let
        notesPerBar =
            TimeSignature.durationsPerBar ts duration

        numBars =
            notesPerBar
                |> Maybe.map (\npb -> (line |> List.length |> toFloat) / toFloat npb |> ceiling)
                |> Maybe.withDefault 0

        numDurationsPerQuarter =
            case Ratio.divideIntBy 4 (Note.toSixteenthNotes duration) |> Ratio.split of
                ( x, 1 ) ->
                    Just x

                _ ->
                    Nothing

        notes : List ( String, String )
        notes =
            notesPerBar
                |> Maybe.andThen (\npb -> line |> List.head |> Maybe.map (\b -> ( npb, b )))
                |> Maybe.andThen (\( a, b ) -> numDurationsPerQuarter |> Maybe.map (\c -> ( a, b, c )))
                |> Maybe.map
                    (\( npb, firstPitch, ndq ) ->
                        line
                            |> List.drop 1
                            |> List.Extra.scanl
                                (\pitch { index } ->
                                    let
                                        i =
                                            index + 1

                                        bar =
                                            i // npb

                                        quarter =
                                            remainderBy npb i // ndq
                                    in
                                    { index = i
                                    , bar = bar
                                    , quarter = quarter
                                    , sixteenth = ((remainderBy npb i - ndq * quarter) |> toFloat) * (Note.toSixteenthNotes duration |> Ratio.toFloat)
                                    , pitch = pitch
                                    }
                                )
                                { index = 0, bar = 0, quarter = 0, sixteenth = 0.0, pitch = firstPitch }
                    )
                |> Maybe.withDefault []
                |> List.filterMap
                    (\{ bar, quarter, sixteenth, pitch } ->
                        pitch
                            |> toScientificPitchNotation
                            |> Maybe.map (\p -> { bar = bar, quarter = quarter, sixteenth = sixteenth, pitch = p })
                    )
                |> List.map
                    (\{ bar, quarter, sixteenth, pitch } ->
                        ( [ bar |> String.fromInt, quarter |> String.fromInt, sixteenth |> String.fromFloat ] |> String.join ":", pitch )
                    )
    in
    Ports.Out.startSequence
        { timeSignature = timeSignature ts
        , loopEnd = (numBars |> String.fromInt) ++ "m"
        , noteLength = noteLength duration
        , notes = notes
        , clickMuted = clickTrackSwitch |> Switch.fold False True
        }


timeSignature : TimeSignature -> ( String, String )
timeSignature (TimeSignature numBeats beatDuration) =
    ( numBeats |> TimeSignature.numberOfBeatsToInt |> String.fromInt, beatDuration |> TimeSignature.beatDurationToInt |> String.fromInt )


noteLength : Duration -> String
noteLength duration =
    case duration of
        Whole ->
            "1n"

        Half ->
            "2n"

        Quarter None ->
            "4n"

        Quarter Triplet ->
            "3n"

        Quarter Dotted ->
            "4n."

        Eighth None ->
            "8n"

        Eighth Triplet ->
            "12n"

        Eighth Dotted ->
            "8n."

        Sixteenth ->
            "16n"


stop : Cmd msg
stop =
    Ports.Out.stopSequence ()


setTempo : Int -> Cmd msg
setTempo tempo =
    Ports.Out.setTempo tempo


muteClick : Cmd msg
muteClick =
    Ports.Out.setClickMute True


unMuteClick : Cmd msg
unMuteClick =
    Ports.Out.setClickMute False


samplesLoaded : msg -> Sub msg
samplesLoaded msg =
    Ports.In.samplesLoaded (always msg)
