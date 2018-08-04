module Types.Pitch
    exposing
        ( Pitch(..)
        , flat
        , semitoneOffset
        , note
        , all
        , displayPitch
        , sharp
        , natural
        , any
        , choice
        , enharmonicEquivalents
        , transpose
        )

import Types.Octave as Octave exposing (..)
import Types.PitchClass as Note exposing (..)
import List.Extra
import Maybe.Extra


type Pitch
    = Pitch PitchClass Octave


note : Pitch -> PitchClass
note (Pitch note _) =
    note


all : List Pitch
all =
    Octave.all
        |> List.concatMap (\octave -> Note.all |> List.map (\note -> Pitch note octave))


semitoneOffset : Pitch -> Semitones
semitoneOffset (Pitch note octave) =
    Note.semitoneOffset note + Octave.number octave * 12


choose : Accidental -> List Pitch -> Maybe Pitch
choose acc pitches =
    pitches |> List.Extra.find (\(Pitch (PitchClass _ accidental) _) -> accidental == acc)


natural : List Pitch -> Maybe Pitch
natural =
    choose Natural


sharp : List Pitch -> Maybe Pitch
sharp =
    choose Sharp


flat : List Pitch -> Maybe Pitch
flat =
    choose Flat


any : List Pitch -> Maybe Pitch
any =
    List.head


transpose : List (List Pitch -> Maybe Pitch) -> Pitch -> Semitones -> Maybe Pitch
transpose choices pitch semitones =
    enharmonicEquivalents (semitoneOffset pitch + semitones)
        |> choice choices


choice : List (List Pitch -> Maybe Pitch) -> List Pitch -> Maybe Pitch
choice choices pitches =
    choices
        |> List.foldl (\choose pitch -> pitch |> Maybe.Extra.orElse (pitches |> choose)) Nothing


enharmonicEquivalents : Semitones -> List Pitch
enharmonicEquivalents semitones =
    let
        remainder =
            semitones % 12

        notes =
            Note.all |> List.filter (Note.semitoneOffset >> (==) remainder)
    in
        notes
            |> List.filterMap
                (\n ->
                    Octave.all
                        |> List.Extra.find (\o -> Octave.number o * 12 + Note.semitoneOffset n == semitones)
                        |> Maybe.map (Pitch n)
                )


displayPitch : Pitch -> String
displayPitch (Pitch note octave) =
    noteToString note ++ (Octave.number octave |> toString)
