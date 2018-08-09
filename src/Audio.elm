module Audio exposing (loadPianoSamples, play, stop, samplesLoaded, setTempo, muteClick, unMuteClick)

import Types.Switch as Switch exposing (Switch)
import Types.Pitch as Pitch exposing (Pitch(..), choice, flat, sharp, natural)
import Types.PitchClass exposing (PitchClass(..), Accidental(..), Letter(..))
import Types.Octave as Octave
import Types.TimeSignature as TimeSignature exposing (TimeSignature(..))
import Ports.Out
import Ports.In
import Types.Note as Note exposing (Duration(..), Altered(..))
import Ratio


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
                |> Maybe.andThen (\npb -> line |> List.head |> Maybe.map ((,) npb))
                |> Maybe.andThen (\( a, b ) -> numDurationsPerQuarter |> Maybe.map ((,,) a b))
                |> Maybe.map
                    (\( npb, firstPitch, ndq ) ->
                        line
                            |> List.drop 1
                            |> List.scanl
                                (\pitch ( i, _, _, _, _ ) ->
                                    let
                                        index =
                                            i + 1

                                        bar =
                                            index // npb

                                        quarter =
                                            rem index npb // ndq
                                    in
                                        ( index
                                        , bar
                                        , quarter
                                        , ((rem index npb - ndq * quarter) |> toFloat) * (Note.toSixteenthNotes duration |> Ratio.toFloat)
                                        , pitch
                                        )
                                )
                                ( 0, 0, 0, 0.0, firstPitch )
                    )
                |> Maybe.withDefault []
                |> List.filterMap (\( _, bar, quarter, sixteenth, pitch ) -> pitch |> toScientificPitchNotation |> Maybe.map (\p -> ( bar, quarter, sixteenth, p )))
                |> List.map
                    (\( bar, quarter, sixteenth, pitch ) ->
                        ( [ bar |> toString, quarter |> toString, sixteenth |> toString ] |> String.join ":", pitch )
                    )
    in
        Ports.Out.startSequence
            { timeSignature = timeSignature ts
            , loopEnd = (numBars |> toString) ++ "m"
            , noteLength = noteLength duration
            , notes = notes
            , clicks = clicks ts duration
            , clickMuted = clickTrackSwitch |> Switch.fold False True
            }


clicks : TimeSignature -> Duration -> List ( String, String )
clicks (TimeSignature numBeats beatDuration) duration =
    case ( numBeats, beatDuration, duration ) of
        ( TimeSignature.Four, TimeSignature.Quarter, Eighth None ) ->
            [ ( "0:0:0", "" ), ( "0:2:0", "" ) ]

        ( TimeSignature.Five, TimeSignature.Quarter, Eighth None ) ->
            [ ( "0:0:0", "" ), ( "0:3:0", "" ) ]

        ( TimeSignature.Six, TimeSignature.Quarter, Eighth None ) ->
            [ ( "0:0:0", "" ), ( "0:3:0", "" ) ]

        ( TimeSignature.Five, TimeSignature.Eighth, Eighth None ) ->
            [ ( "0:0:0", "" ), ( "0:1:2", "" ) ]

        ( TimeSignature.Seven, TimeSignature.Eighth, Eighth None ) ->
            [ ( "0:0:0", "" ), ( "0:2:0", "" ) ]

        ( TimeSignature.Twelve, TimeSignature.Eighth, Eighth None ) ->
            [ ( "0:0:0", "" ), ( "0:3:0", "" ) ]

        _ ->
            [ ( "0:0:0", "" ) ]


timeSignature : TimeSignature -> ( String, String )
timeSignature (TimeSignature numBeats beatDuration) =
    ( numBeats |> TimeSignature.numberOfBeatsToInt |> toString, beatDuration |> TimeSignature.beatDurationToInt |> toString )


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
