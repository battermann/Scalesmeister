module Types.Pitch exposing (..)

import Types.Octave as Octave exposing (..)
import Types.Note as Note exposing (..)


type Pitch
    = Pitch Note Octave


type alias PitchNotation =
    String


note : Pitch -> Note
note (Pitch note _) =
    note


all : List Pitch
all =
    Octave.all
        |> List.concatMap (\octave -> Note.all |> List.map (\note -> Pitch note octave))


semitoneOffset : Pitch -> Semitones
semitoneOffset (Pitch note octave) =
    Note.semitoneOffset note + (Octave.number octave) * 12
