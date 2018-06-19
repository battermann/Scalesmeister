module Types.Range exposing (..)

import Types.Pitch as Pitch exposing (..)
import Types.Note exposing (..)
import Types.Octave as Octave exposing (..)


type Range
    = OfPitch
        { lowest : Pitch
        , highest : Pitch
        }
    | OfSemitones
        { lowest : Semitones
        , highest : Semitones
        }


toSemitoneOffsetRange : Range -> ( Semitones, Semitones )
toSemitoneOffsetRange range =
    case range of
        OfPitch { lowest, highest } ->
            ( Pitch.semitoneOffset lowest, Pitch.semitoneOffset highest )

        OfSemitones { lowest, highest } ->
            ( lowest, highest )


contains : Pitch -> Range -> Bool
contains pitch range =
    let
        ( lowest, highest ) =
            toSemitoneOffsetRange range
    in
        lowest <= Pitch.semitoneOffset pitch && highest >= Pitch.semitoneOffset pitch


doubleBass : Range
doubleBass =
    OfPitch
        { lowest = Pitch (Note E Natural) Octave.one
        , highest = Pitch (Note G Natural) Octave.four
        }


piano : Range
piano =
    OfPitch
        { lowest = Pitch (Note A Natural) Octave.zero
        , highest = Pitch (Note C Natural) Octave.eight
        }
