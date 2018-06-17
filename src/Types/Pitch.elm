module Types.Pitch exposing (..)

import Array exposing (Array)
import Types.Octave exposing (..)
import Types.Note exposing (..)


type Pitch
    = Pitch Note Octave


type alias PitchNotation =
    String


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
