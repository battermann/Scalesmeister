module Types exposing (..)

import Array exposing (Array)


type Accidental
    = Sharp
    | Flat


type alias Octave =
    Int


type Note
    = C
    | D
    | E
    | F
    | G
    | A
    | B


type alias Pitch =
    { note : Note
    , accidental : Maybe Accidental
    , octave : Octave
    }


type alias PitchNotation =
    String


type alias SampleUrl =
    String


type Row
    = Stopped (Array Pitch)
    | Playing (Array Pitch)


type alias Model =
    Maybe Row
