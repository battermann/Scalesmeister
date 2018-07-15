module Types.Note exposing (..)

import Types.Pitch exposing (..)


type Duration
    = Whole
    | Half
    | Quarter
    | Eighth
    | Sixteenth


numberOfSixteenth : Duration -> Int
numberOfSixteenth duration =
    case duration of
        Whole ->
            16

        Half ->
            8

        Quarter ->
            4

        Eighth ->
            2

        Sixteenth ->
            1


type Note
    = Note Pitch Duration


type Rest
    = Rest Duration
