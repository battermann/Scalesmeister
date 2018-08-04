module Types.Note exposing (..)

import Types.Pitch exposing (..)
import Ratio exposing (..)


type Altered
    = None
    | Triplet
    | Dotted


type Duration
    = Whole
    | Half
    | Quarter Altered
    | Eighth Altered
    | Sixteenth


addDurations : Duration -> Duration -> Maybe Duration
addDurations d1 d2 =
    toSixteenthNotes d1
        |> add (toSixteenthNotes d2)
        |> fromSixteenthNotes


fromSixteenthNotes : Rational -> Maybe Duration
fromSixteenthNotes duration =
    case ( numerator duration, denominator duration ) of
        ( 16, 1 ) ->
            Just Whole

        ( 8, 1 ) ->
            Just Half

        ( 4, 1 ) ->
            Just (Quarter None)

        ( 8, 3 ) ->
            Just (Quarter Triplet)

        ( 6, 1 ) ->
            Just (Quarter Dotted)

        ( 2, 1 ) ->
            Just (Eighth None)

        ( 4, 3 ) ->
            Just (Eighth Triplet)

        ( 3, 1 ) ->
            Just (Eighth Dotted)

        ( 1, 1 ) ->
            Just Sixteenth

        _ ->
            Nothing


toSixteenthNotes : Duration -> Rational
toSixteenthNotes duration =
    case duration of
        Whole ->
            fromInt 16

        Half ->
            fromInt 8

        Quarter None ->
            fromInt 4

        Quarter Triplet ->
            over 8 3

        Quarter Dotted ->
            fromInt 6

        Eighth None ->
            fromInt 2

        Eighth Triplet ->
            over 4 3

        Eighth Dotted ->
            fromInt 3

        Sixteenth ->
            fromInt 1


type Note
    = Note Pitch Duration


type Rest
    = Rest Duration
