module Types.PitchClass
    exposing
        ( Accidental(..)
        , PitchClass(..)
        , pitchClassToString
        , transpose
        , down
        , Semitones
        , semitoneOffset
        , all
        , accidental
        , Letter(..)
        )

import Types.Interval as Interval exposing (Interval, IntervalNumber(..))
import List.Extra


type Accidental
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp


type Letter
    = C
    | D
    | E
    | F
    | G
    | A
    | B


type PitchClass
    = PitchClass Letter Accidental


accidental : PitchClass -> Accidental
accidental (PitchClass _ accidental) =
    accidental


type alias Semitones =
    Int


letters : List Letter
letters =
    [ C, D, E, F, G, A, B ]


all : List PitchClass
all =
    letters
        |> List.concatMap (\letter -> [ PitchClass letter DoubleFlat, PitchClass letter Flat, PitchClass letter Natural, PitchClass letter Sharp ])


semitoneOffset : PitchClass -> Semitones
semitoneOffset (PitchClass letter accidental) =
    letterSemitoneOffset letter + accidentalSemitoneOffset accidental


letterSemitoneOffset : Letter -> Semitones
letterSemitoneOffset letter =
    case letter of
        C ->
            0

        D ->
            2

        E ->
            4

        F ->
            5

        G ->
            7

        A ->
            9

        B ->
            11


accidentalSemitoneOffset : Accidental -> Semitones
accidentalSemitoneOffset accidental =
    case accidental of
        DoubleFlat ->
            -2

        Flat ->
            -1

        Natural ->
            0

        Sharp ->
            1

        DoubleSharp ->
            2


addIntervalSizeToLetter : Letter -> IntervalNumber -> Maybe Letter
addIntervalSizeToLetter letter intervalSize =
    letters
        ++ letters
        |> List.Extra.dropWhile ((/=) letter)
        |> List.Extra.zip [ Unison, Second, Third, Fourth, Fifth, Sixth, Seventh ]
        |> List.Extra.find (Tuple.first >> (==) intervalSize)
        |> Maybe.map Tuple.second


noteLetterDistance : PitchClass -> Letter -> Semitones
noteLetterDistance (PitchClass letter accidental) targetLetter =
    let
        rootOffset =
            letterSemitoneOffset letter + accidentalSemitoneOffset accidental

        targetOffset =
            letterSemitoneOffset targetLetter
    in
        if rootOffset < targetOffset then
            targetOffset - rootOffset
        else
            12 - rootOffset + targetOffset


accidentalBySemitoneOffset : Semitones -> Maybe Accidental
accidentalBySemitoneOffset semitones =
    case semitones of
        (-2) ->
            Just DoubleFlat

        (-1) ->
            Just Flat

        0 ->
            Just Natural

        1 ->
            Just Sharp

        2 ->
            Just DoubleSharp

        _ ->
            Nothing


down : (Interval -> PitchClass -> Maybe PitchClass) -> (Interval -> PitchClass -> Maybe PitchClass)
down f =
    \interval note -> f (Interval.complementary interval) note


transpose : Interval -> PitchClass -> Maybe PitchClass
transpose interval (PitchClass letter accidental) =
    let
        intervalNumber =
            Interval.number interval

        semitones =
            Interval.semitones interval

        maybeTargetLetter =
            addIntervalSizeToLetter letter intervalNumber
    in
        maybeTargetLetter
            |> Maybe.map (noteLetterDistance (PitchClass letter accidental))
            |> Maybe.map ((-) semitones)
            |> Maybe.andThen accidentalBySemitoneOffset
            |> Maybe.andThen (\acc -> maybeTargetLetter |> Maybe.map (\l -> PitchClass l acc))


accidentalToString : Accidental -> String
accidentalToString accidental =
    case accidental of
        DoubleFlat ->
            "𝄫"

        Flat ->
            "♭"

        Natural ->
            ""

        Sharp ->
            "♯"

        DoubleSharp ->
            "𝄪"


pitchClassToString : PitchClass -> String
pitchClassToString (PitchClass letter accidental) =
    toString letter ++ (accidental |> accidentalToString)
