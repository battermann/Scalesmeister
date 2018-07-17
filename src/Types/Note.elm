module Types.Note exposing (..)

import Types.Pitch exposing (..)
import Ratio exposing (..)


type Altered
    = None
    | Triplet


type Duration
    = Whole
    | Half
    | Quarter
    | Eighth Altered
    | Sixteenth


durationSixteenthNoteRational : Duration -> Rational
durationSixteenthNoteRational duration =
    case duration of
        Whole ->
            fromInt 16

        Half ->
            fromInt 8

        Quarter ->
            fromInt 4

        Eighth None ->
            fromInt 2

        Eighth Triplet ->
            over 4 3

        Sixteenth ->
            fromInt 1


type Note
    = Note Pitch Duration


type Rest
    = Rest Duration
