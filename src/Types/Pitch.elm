module Types.Pitch exposing (..)

import Types.Octave as Octave exposing (..)
import Types.Note as Note exposing (..)
import List.Extra


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


toClosestEnharmonicEquivalent : Pitch -> Maybe Pitch
toClosestEnharmonicEquivalent (Pitch (Note letter acc) octave) =
    case acc of
        Natural ->
            Just (Pitch (Note letter acc) octave)

        Flat ->
            Just (Pitch (Note letter acc) octave)

        Sharp ->
            Just (Pitch (Note letter acc) octave)

        DoubleFlat ->
            all
                |> List.Extra.find
                    (\pitch ->
                        semitoneOffset pitch == semitoneOffset (Pitch (Note letter acc) octave) && (accidental pitch == Natural || accidental pitch == Flat)
                    )

        DoubleSharp ->
            all
                |> List.Extra.find
                    (\pitch ->
                        semitoneOffset pitch == semitoneOffset (Pitch (Note letter acc) octave) && (accidental pitch == Natural || accidental pitch == Sharp)
                    )
