module Types.Interval exposing (IntervalSize(..), IntervalQuality(..), Interval, addIntervalSizeToLetter, noteLetterDistance, addIntervalToNote, perfectUnison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, augmentedFourth, diminishedFifth, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh)

import Types.Note as Note exposing (..)
import List.Extra


type IntervalSize
    = Unison
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh


type IntervalQuality
    = Diminished
    | Minor
    | Perfect
    | Major
    | Augmented


type Interval
    = Interval IntervalQuality IntervalSize Semitones


addIntervalSizeToLetter : Letter -> IntervalSize -> Maybe Letter
addIntervalSizeToLetter letter intervalSize =
    Note.letters
        ++ Note.letters
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


addIntervalToNote : Note -> Interval -> Maybe Note
addIntervalToNote (Note letter accidental) (Interval intervalQuality intervalSize semitones) =
    let
        maybeTargetLetter =
            addIntervalSizeToLetter letter intervalSize
    in
        maybeTargetLetter
            |> Maybe.map (noteLetterDistance (Note letter accidental))
            |> Maybe.map ((-) semitones)
            |> Maybe.andThen accidentalBySemitoneOffset
            |> Maybe.andThen
                (\accidental ->
                    maybeTargetLetter |> Maybe.map (\letter -> (Note letter accidental))
                )


perfectUnison : Interval
perfectUnison =
    Interval Perfect Unison 0


minorSecond : Interval
minorSecond =
    Interval Minor Second 1


majorSecond : Interval
majorSecond =
    Interval Major Second 2


minorThird : Interval
minorThird =
    Interval Minor Third 3


majorThird : Interval
majorThird =
    Interval Major Third 4


perfectFourth : Interval
perfectFourth =
    Interval Perfect Fourth 5


augmentedFourth : Interval
augmentedFourth =
    Interval Augmented Fourth 6


diminishedFifth : Interval
diminishedFifth =
    Interval Diminished Fifth 6


perfectFifth : Interval
perfectFifth =
    Interval Perfect Fifth 7


augmentedFifth : Interval
augmentedFifth =
    Interval Augmented Fifth 8


minorSixth : Interval
minorSixth =
    Interval Minor Sixth 8


majorSixth : Interval
majorSixth =
    Interval Major Sixth 9


minorSeventh : Interval
minorSeventh =
    Interval Minor Seventh 10


majorSeventh : Interval
majorSeventh =
    Interval Major Seventh 11
