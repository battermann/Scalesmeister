module Types.Pitch exposing (..)

import Types.Octave as Octave exposing (..)
import Types.Note as Note exposing (..)
import Types.Interval as Interval exposing (..)
import List.Extra
import Maybe.Extra


type Pitch
    = Pitch Note Octave


type alias PitchNotation =
    String


note : Pitch -> Note
note (Pitch note _) =
    note


accidental : Pitch -> Accidental
accidental (Pitch note _) =
    Note.accidental note


all : List Pitch
all =
    Octave.all
        |> List.concatMap (\octave -> Note.all |> List.map (\note -> Pitch note octave))


semitoneOffset : Pitch -> Semitones
semitoneOffset (Pitch note octave) =
    Note.semitoneOffset note + (Octave.number octave) * 12


natural : List Pitch -> Maybe Pitch
natural pitches =
    pitches |> List.Extra.find (\(Pitch (Note _ accidental) _) -> accidental == Natural)


sharp : List Pitch -> Maybe Pitch
sharp pitches =
    pitches |> List.Extra.find (\(Pitch (Note _ accidental) _) -> accidental == Sharp)


flat : List Pitch -> Maybe Pitch
flat pitches =
    pitches |> List.Extra.find (\(Pitch (Note _ accidental) _) -> accidental == Flat)


transpose : List (List Pitch -> Maybe Pitch) -> Pitch -> Semitones -> Maybe Pitch
transpose xs pitch semitones =
    let
        all =
            enharmonicEquivalents (semitoneOffset pitch + semitones)
    in
        xs
            |> List.foldl (\choose b -> b |> Maybe.Extra.orElse (all |> choose)) Nothing
            |> Maybe.Extra.orElse (all |> List.head)


enharmonicEquivalents : Semitones -> List Pitch
enharmonicEquivalents semitones =
    let
        octave =
            semitones // 12

        remainder =
            semitones % 12

        notes =
            Note.all |> List.filter (Note.semitoneOffset >> ((==) remainder))
    in
        notes
            |> List.filterMap
                (\n ->
                    Octave.all
                        |> List.Extra.find (\o -> (Octave.number o) * 12 + (Note.semitoneOffset n) == semitones)
                        |> Maybe.map (Pitch n)
                )
