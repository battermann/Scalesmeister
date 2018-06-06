module Types exposing (..)


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


type alias Model =
    Maybe Pitch
