module Types.Pitch exposing
    ( Pitch(..)
    , all
    , choice
    , enharmonicEquivalents
    , flat
    , natural
    , pitchClass
    , semitones
    , sharp
    , simpleSpelling
    , toString
    , transpose
    )

import List.Extra
import Maybe.Extra
import MusicTheory.Internals.PitchClass as Internal
import MusicTheory.Letter as Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.PitchClass.Spelling as Spelling exposing (Accidental(..))
import Types.Octave as Octave exposing (Octave)


type Pitch
    = Pitch PitchClass Octave


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
                Octave.up octave

            else if semitonesSimplified > semitonesOriginal then
                Octave.down octave

            else
                octave
    in
    ( spelling.letter, spelling.accidental, octaveSimplified )


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
        |> List.foldl (\c pitch -> pitch |> Maybe.Extra.orElse (pitches |> c)) Nothing


transpose : List (List Pitch -> Maybe Pitch) -> Pitch -> Int -> Maybe Pitch
transpose choices pitch theSemitones =
    enharmonicEquivalents (semitones pitch + theSemitones)
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
    (Spelling.simple pc |> Spelling.toString) ++ (Octave.number octave |> String.fromInt)
