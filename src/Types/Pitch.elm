module Types.Pitch exposing
    ( Pitch
    , all
    , choice
    , enharmonicEquivalents
    , flat
    , natural
    , pitch
    , pitchClass
    , semitones
    , sharp
    , simpleSpelling
    , toString
    , transposeDown
    , transposeOld
    , transposeUp
    )

import List.Extra
import Maybe.Extra
import MusicTheory.Internals.PitchClass as Internal
import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.Letter as Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.PitchClass.Spelling as Spelling exposing (Accidental(..))
import Types.Octave as Octave exposing (Octave)


type Pitch
    = Pitch PitchClass Octave


pitch : PitchClass -> Octave -> Pitch
pitch pc octave =
    Pitch pc octave


pitchClass : Pitch -> PitchClass
pitchClass (Pitch n _) =
    n


all : List Pitch
all =
    Octave.all
        |> List.concatMap (\octave -> PitchClass.all |> List.map (\pc -> Pitch pc octave))


semitones : Pitch -> Int
semitones (Pitch pc octave) =
    PitchClass.semitones pc + Octave.number octave * 12


simpleSpelling : Pitch -> ( Letter, Accidental, Octave )
simpleSpelling (Pitch pc octave) =
    -- todo: fixme
    let
        spelling =
            Spelling.simple pc

        accSemitones =
            case spelling.accidental of
                Flat ->
                    -1

                Natural ->
                    0

                Sharp ->
                    1

        semitonesSimplified =
            Letter.semitones spelling.letter + accSemitones + (Octave.number octave * 12)

        semitonesOriginal =
            semitones (Pitch pc octave)

        octaveSimplified =
            if semitonesSimplified < semitonesOriginal then
                Octave.up octave |> Maybe.withDefault Octave.one

            else if semitonesSimplified > semitonesOriginal then
                Octave.down octave |> Maybe.withDefault Octave.one

            else
                octave
    in
    ( spelling.letter, spelling.accidental, octave )


choose : Accidental -> List Pitch -> Maybe Pitch
choose acc pitches =
    let
        offset =
            case acc of
                Flat ->
                    -1

                Natural ->
                    0

                Sharp ->
                    1
    in
    pitches |> List.Extra.find (\(Pitch pc _) -> Internal.offset pc == offset)


flat : List Pitch -> Maybe Pitch
flat =
    choose Flat


sharp : List Pitch -> Maybe Pitch
sharp =
    choose Sharp


natural : List Pitch -> Maybe Pitch
natural =
    choose Natural


choice : List (List Pitch -> Maybe Pitch) -> List Pitch -> Maybe Pitch
choice choices pitches =
    choices
        |> List.foldl (\c p -> p |> Maybe.Extra.orElse (pitches |> c)) Nothing


transposeOld : List (List Pitch -> Maybe Pitch) -> Pitch -> Int -> Maybe Pitch
transposeOld choices p theSemitones =
    enharmonicEquivalents (semitones p + theSemitones)
        |> choice choices


enharmonicEquivalents : Int -> List Pitch
enharmonicEquivalents theSemitones =
    let
        remainder =
            modBy 12 theSemitones

        notes =
            PitchClass.all |> List.filter (PitchClass.semitones >> (==) remainder)
    in
    notes
        |> List.filterMap
            (\n ->
                Octave.all
                    |> List.Extra.find (\o -> Octave.number o * 12 + PitchClass.semitones n == theSemitones)
                    |> Maybe.map (Pitch n)
            )


toString : Pitch -> String
toString (Pitch pc octave) =
    -- todo: fixme
    (Spelling.simple pc |> Spelling.toString) ++ (Octave.number octave |> String.fromInt)


transposeUp : Interval -> Pitch -> Maybe Pitch
transposeUp interval (Pitch pc octave) =
    let
        targetPitchClass =
            PitchClass.transposeUp interval pc
                |> max3Accidentals

        targetSemitones =
            semitones (Pitch pc octave) + Interval.semitones interval
    in
    Octave.all |> List.map (Pitch targetPitchClass) |> List.Extra.find (semitones >> (==) targetSemitones)


transposeDown : Interval -> Pitch -> Maybe Pitch
transposeDown interval (Pitch pc octave) =
    let
        targetPitchClass =
            PitchClass.transposeDown interval pc
                |> max3Accidentals

        targetSemitones =
            semitones (Pitch pc octave) + Interval.semitones interval
    in
    Octave.all |> List.map (Pitch targetPitchClass) |> List.Extra.find (semitones >> (==) targetSemitones)



-- ENHARMONIC
-- todo: move to pitch class


max3Accidentals : PitchClass -> PitchClass
max3Accidentals pc =
    if Internal.offset pc < -3 then
        PitchClass.simple pc

    else if Internal.offset pc > 3 then
        PitchClass.simple pc

    else
        pc
