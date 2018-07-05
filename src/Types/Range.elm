module Types.Range exposing (..)

import Types.Pitch as Pitch exposing (..)
import Types.Note exposing (..)
import Types.Octave as Octave exposing (..)


type alias Range =
    { lowest : Pitch
    , highest : Pitch
    }


setLowest : Range -> Pitch -> Range
setLowest range pitch =
    { range | lowest = pitch }


setHighest : Range -> Pitch -> Range
setHighest range pitch =
    { range | highest = pitch }


toSemitoneOffsetRange : Range -> ( Semitones, Semitones )
toSemitoneOffsetRange range =
    ( Pitch.semitoneOffset range.lowest, Pitch.semitoneOffset range.highest )


contains : Pitch -> Range -> Bool
contains pitch range =
    let
        ( lowest, highest ) =
            toSemitoneOffsetRange range
    in
        lowest <= Pitch.semitoneOffset pitch && highest >= Pitch.semitoneOffset pitch


doubleBass : Range
doubleBass =
    { lowest = Pitch (Note E Natural) Octave.one
    , highest = Pitch (Note G Natural) Octave.four
    }


piano : Range
piano =
    { lowest = Pitch (Note A Natural) Octave.zero
    , highest = Pitch (Note C Natural) Octave.eight
    }
