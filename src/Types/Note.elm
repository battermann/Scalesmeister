module Types.Note exposing (..)

import Types.Interval as Interval exposing (..)
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


type Note
    = Note Letter Accidental


type alias PitchClass =
    Note


accidental : Note -> Accidental
accidental (Note _ accidental) =
    accidental


type alias Semitones =
    Int


letters : List Letter
letters =
    [ C, D, E, F, G, A, B ]


all : List Note
all =
    letters
        |> List.concatMap (\letter -> [ Note letter DoubleFlat, Note letter Flat, Note letter Natural, Note letter Sharp ])


semitoneOffset : Note -> Semitones
semitoneOffset (Note letter accidental) =
    (letterSemitoneOffset letter) + (accidentalSemitoneOffset accidental)


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
        |> List.Extra.find (Tuple.first >> ((==) intervalSize))
        |> Maybe.map Tuple.second


noteLetterDistance : Note -> Letter -> Semitones
noteLetterDistance (Note letter accidental) targetLetter =
    let
        rootOffset =
            (letterSemitoneOffset letter) + (accidentalSemitoneOffset accidental)

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


down : (Interval -> Note -> Maybe Note) -> (Interval -> Note -> Maybe Note)
down f =
    (\interval note -> f (Interval.complementary interval) note)


transpose : Interval -> Note -> Maybe Note
transpose interval (Note letter accidental) =
    let
        intervalQuality =
            Interval.quality interval

        intervalNumber =
            Interval.number interval

        semitones =
            Interval.semitones interval

        maybeTargetLetter =
            addIntervalSizeToLetter letter intervalNumber
    in
        maybeTargetLetter
            |> Maybe.map (noteLetterDistance (Note letter accidental))
            |> Maybe.map ((-) semitones)
            |> Maybe.andThen accidentalBySemitoneOffset
            |> Maybe.andThen
                (\accidental ->
                    maybeTargetLetter |> Maybe.map (\letter -> (Note letter accidental))
                )


enharmonicEquivalents : Note -> List Note
enharmonicEquivalents note =
    all |> List.filter (\n -> (semitoneOffset n) % 12 == (semitoneOffset note) % 2)
