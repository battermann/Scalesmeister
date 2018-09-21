module Types.PitchClass exposing
    ( Accidental(..)
    , Letter(..)
    , PitchClass(..)
    , Semitones
    , accidental
    , addIntervalSizeToLetter
    , all
    , down
    , noteLetterDistance
    , pitchClassToString
    , semitoneOffset
    , transpose
    )

import List.Extra
import Types.Interval as Interval exposing (Interval, IntervalNumber(..))


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
accidental (PitchClass _ acc) =
    acc


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
semitoneOffset (PitchClass letter acc) =
    letterSemitoneOffset letter + accidentalSemitoneOffset acc


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
accidentalSemitoneOffset acc =
    case acc of
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
noteLetterDistance (PitchClass letter acc) targetLetter =
    let
        rootOffset =
            letterSemitoneOffset letter + accidentalSemitoneOffset acc

        targetOffset =
            letterSemitoneOffset targetLetter
    in
    if rootOffset < targetOffset then
        targetOffset - rootOffset

    else
        12 - rootOffset + targetOffset


accidentalBySemitoneOffset : Semitones -> Maybe Accidental
accidentalBySemitoneOffset semitones =
    if semitones == -2 then
        Just DoubleFlat

    else if semitones == -1 then
        Just Flat

    else if semitones == 0 then
        Just Natural

    else if semitones == 1 then
        Just Sharp

    else if semitones == 2 then
        Just DoubleSharp

    else
        Nothing


down : (Interval -> PitchClass -> Maybe PitchClass) -> (Interval -> PitchClass -> Maybe PitchClass)
down f =
    \interval note -> f (Interval.complementary interval) note


transpose : Interval -> PitchClass -> Maybe PitchClass
transpose interval (PitchClass letter acc) =
    let
        intervalNumber =
            Interval.number interval

        semitones =
            Interval.semitones interval

        maybeTargetLetter =
            addIntervalSizeToLetter letter intervalNumber
    in
    maybeTargetLetter
        |> Maybe.map (noteLetterDistance (PitchClass letter acc))
        |> Maybe.map ((-) semitones)
        |> Maybe.andThen accidentalBySemitoneOffset
        |> Maybe.andThen (\a -> maybeTargetLetter |> Maybe.map (\l -> PitchClass l a))


accidentalToString : Accidental -> String
accidentalToString acc =
    case acc of
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


letterToString : Letter -> String
letterToString letter =
    case letter of
        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        A ->
            "A"

        B ->
            "B"


pitchClassToString : PitchClass -> String
pitchClassToString (PitchClass letter acc) =
    letterToString letter ++ (acc |> accidentalToString)
