module Octave exposing (..)


type Octave
    = Octave Int


octave : Int -> Maybe Octave
octave n =
    if (n >= 0 && n <= 8) then
        Octave n |> Just
    else
        Nothing


number : Octave -> Int
number (Octave n) =
    n


middleOctave : Octave
middleOctave =
    Octave 4
