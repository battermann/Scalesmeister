module MusicTheory.PitchClass exposing
    ( PitchClass
    , all
    , areEnharmonicEqual
    , asNaturalOrElseFlat, asNaturalOrElseSharp, doubleFlat, doubleSharp, enharmonicEquivalents, flat, natural, pitchClass, semitones, sharp, simple, toString, transposeDown, transposeUp, tripleFlat, tripleSharp
    )

{-| A pitch class is a set of all pitches that are a whole number of octaves apart. A pitch class is represented as a letter together with an accidental.

The internals of `PitchClass` are opaque. By using accessor functions the caller can get specific enharmonic equivalent pitch classes depending on their needs.


# Definition

@docs PitchClass


# Constructors

@docs pitchClass fromTuple


# Accessors

@docs all


# Transform

@docs transposeUp transposeDown transposeBySemitones semitones


# Comparison

@docs areEnharmonicEqual

-}

import MusicTheory.Internals.PitchClass as Internal
import MusicTheory.Internals.PitchClass.Enharmonic as EnharmonicInternal exposing (NaturalOrSingleAccidental(..))
import MusicTheory.Interval as Interval exposing (Interval, IntervalNumber(..), IntervalQuality(..))
import MusicTheory.Letter as Letter exposing (Letter(..))



-- DEFINITION


{-| Internal representation of an unlimited raised or lowered letter. E.g. four flats are represented as as `Offset -4`.
-}
type alias Offset =
    Internal.Offset


{-| Opaque type that represents a pitch class.
-}
type alias PitchClass =
    Internal.PitchClass



-- CONSTRUCTORS


tripleFlat : Offset
tripleFlat =
    Internal.Offset -3


doubleFlat : Offset
doubleFlat =
    Internal.Offset -2


flat : Offset
flat =
    Internal.Offset -1


natural : Offset
natural =
    Internal.Offset 0


sharp : Offset
sharp =
    Internal.Offset 1


doubleSharp : Offset
doubleSharp =
    Internal.Offset 2


tripleSharp : Offset
tripleSharp =
    Internal.Offset 3


{-| Create a pitch class from a letter and an accidental.

    pitchClass C Sharp -- creates the pitch class C♯

-}
pitchClass : Letter -> Offset -> PitchClass
pitchClass letter offset =
    Internal.PitchClass letter offset



-- ACCESSORS


{-| A list of all pitch classes that with at most 3 accidentals. `Cbbb, Cbb, Cb, C, C#, C##, C###, Dbbb, Dbb, ...`.
-}
all : List PitchClass
all =
    Letter.letters
        |> List.concatMap (\l -> [ tripleFlat, doubleFlat, flat, natural, sharp, doubleSharp, tripleSharp ] |> List.map (pitchClass l))



-- TRANSFORM


{-| Number of semitones between C natural and a given pitch class.

    semitones (pitchClass E Natural) == 4

-}
semitones : PitchClass -> Int
semitones pc =
    exactSemitones pc |> modBy 12


toString : PitchClass -> String
toString pc =
    internalToString (simple pc)



-- TRANSPOSE


{-| Moves a pitch class up by a given interval while taking the correct number off staff positions between root and target pitch class into account.

    (pitchClass C Sharp |> transposeUp Interval.majorSecond |> exact) == Just (D, Sharp)

    The result might not be representable in terms of the correct letter and a valid accidental. In this case a suitable enharmonic representation can be retrieved by applying `asNaturalOrLoweredOnce` or `asNaturalOrRaisedOnce`.

    (pitchClass C TripleFlat |> transposeUp Interval.minorSecond |> exact) == Nothing

    (pitchClass C TripleFlat |> transposeUp Interval.minorSecond |> asNaturalOrLoweredOnce) == (B, Flat)

-}
transposeUp : Interval -> PitchClass -> PitchClass
transposeUp interval pc =
    let
        ( targetLetter, letterToLetterDistance ) =
            targetLetterWithSemitoneDistance (Letter.index (Internal.letter pc)) (Interval.intervalNumberIndex (Interval.number interval)) ( Internal.letter pc, 0 )
    in
    Internal.PitchClass targetLetter (Internal.Offset (Interval.semitones interval - letterToLetterDistance + Internal.offset pc))


{-| Moves a pitch class down by a given interval while taking the correct number off staff positions between root and target pitch class into account.

    (pitchClass B Natural |> transposeDown Interval.minorSecond) == pitchClass A Sharp

-}
transposeDown : Interval -> PitchClass -> PitchClass
transposeDown interval pc =
    interval
        |> Interval.complementary
        |> (\i -> transposeUp i pc)



-- COMPARISON


{-| Returns true if two pitch classes are enharmonic equivalent.
-}
areEnharmonicEqual : PitchClass -> PitchClass -> Bool
areEnharmonicEqual lhs rhs =
    semitones lhs == semitones rhs



-- ENHARMONIC


enharmonicEquivalents : PitchClass -> List PitchClass
enharmonicEquivalents pc =
    all |> List.filter (semitones >> (==) (semitones pc))


simple : PitchClass -> PitchClass
simple pc =
    if Internal.offset pc == 0 then
        pc

    else if Internal.offset pc < 0 then
        asNaturalOrElseFlat pc

    else
        asNaturalOrElseSharp pc


asNaturalOrElseFlat : PitchClass -> PitchClass
asNaturalOrElseFlat pc =
    case pc |> semitones |> EnharmonicInternal.semitonesToNaturalOrAccidental of
        Nat letter ->
            pitchClass letter natural

        SharpFlat _ letter ->
            pitchClass letter flat


asNaturalOrElseSharp : PitchClass -> PitchClass
asNaturalOrElseSharp pc =
    case pc |> semitones |> EnharmonicInternal.semitonesToNaturalOrAccidental of
        Nat letter ->
            pitchClass letter natural

        SharpFlat letter _ ->
            pitchClass letter sharp



-- INTERNALS


internalToString : PitchClass -> String
internalToString pc =
    case pc of
        Internal.PitchClass letter (Internal.Offset offset) ->
            if offset == 0 then
                Letter.toString letter

            else if offset < 0 then
                Letter.toString letter ++ (List.repeat (abs offset) "♭" |> String.join "")

            else
                Letter.toString letter ++ (List.repeat (abs offset) "♯" |> String.join "")


exactSemitones : PitchClass -> Int
exactSemitones (Internal.PitchClass letter (Internal.Offset offset)) =
    Letter.semitones letter + offset


targetLetterWithSemitoneDistance : Int -> Int -> ( Letter, Int ) -> ( Letter, Int )
targetLetterWithSemitoneDistance currentIndex steps ( currentLetter, totalSemitones ) =
    if steps <= 0 then
        ( currentLetter, totalSemitones )

    else
        let
            ( currentTargetLetter, stepSemitones ) =
                Letter.indexToLetterAndSteps (currentIndex + 1)
        in
        targetLetterWithSemitoneDistance (currentIndex + 1) (steps - 1) ( currentTargetLetter, totalSemitones + stepSemitones )
