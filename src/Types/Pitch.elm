module Types.Pitch exposing (..)

import Array exposing (Array)
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


chromaticScale : Octave -> Array Pitch
chromaticScale octave =
    Array.fromList
        [ Pitch (Note C Natural) octave
        , Pitch (Note C Sharp) octave
        , Pitch (Note C Natural) octave
        , Pitch (Note D Sharp) octave
        , Pitch (Note E Natural) octave
        , Pitch (Note F Natural) octave
        , Pitch (Note F Sharp) octave
        , Pitch (Note G Natural) octave
        , Pitch (Note G Sharp) octave
        , Pitch (Note A Natural) octave
        , Pitch (Note A Sharp) octave
        , Pitch (Note B Natural) octave
        ]
