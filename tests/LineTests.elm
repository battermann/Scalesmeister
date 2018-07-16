module LineTests exposing (..)

import Test exposing (..)
import Expect
import Types.Scale exposing (..)
import Types.PitchClass exposing (..)
import Types.Line as Line exposing (..)
import Types.Range as Range exposing (..)
import Types.Octave as Octave exposing (..)
import Types.Pitch as Pitch exposing (..)
import Types.Formula as Formula exposing (..)


middleOctaveRange : Range
middleOctaveRange =
    Range.piano
        |> Range.setLowest (Pitch (PitchClass C Natural) Octave.four)
        |> Range.setHighest (Pitch (PitchClass B Natural) Octave.four)


line : Line
line =
    [ Pitch (PitchClass B Flat) Octave.three
    , Pitch (PitchClass C Natural) Octave.four
    , Pitch (PitchClass E Flat) Octave.four
    , Pitch (PitchClass F Natural) Octave.four
    , Pitch (PitchClass G Natural) Octave.four
    , Pitch (PitchClass B Flat) Octave.four
    , Pitch (PitchClass C Natural) Octave.five
    , Pitch (PitchClass E Flat) Octave.five
    , Pitch (PitchClass F Natural) Octave.five
    , Pitch (PitchClass G Natural) Octave.five
    , Pitch (PitchClass B Flat) Octave.five
    , Pitch (PitchClass C Natural) Octave.six
    ]


formula : Formula
formula =
    [ -2, -1, 2, -1 ]


expected : Line
expected =
    [ Pitch (PitchClass C Natural) Octave.six
    , Pitch (PitchClass G Natural) Octave.five
    , Pitch (PitchClass F Natural) Octave.five
    , Pitch (PitchClass B Flat) Octave.five
    , Pitch (PitchClass G Natural) Octave.five
    , Pitch (PitchClass E Flat) Octave.five
    , Pitch (PitchClass C Natural) Octave.five
    , Pitch (PitchClass F Natural) Octave.five
    , Pitch (PitchClass E Flat) Octave.five
    , Pitch (PitchClass B Flat) Octave.four
    , Pitch (PitchClass G Natural) Octave.four
    , Pitch (PitchClass C Natural) Octave.five
    , Pitch (PitchClass B Flat) Octave.four
    , Pitch (PitchClass F Natural) Octave.four
    , Pitch (PitchClass E Flat) Octave.four
    , Pitch (PitchClass G Natural) Octave.four
    , Pitch (PitchClass F Natural) Octave.four
    , Pitch (PitchClass C Natural) Octave.four
    , Pitch (PitchClass B Flat) Octave.three
    , Pitch (PitchClass E Flat) Octave.four
    , Pitch (PitchClass C Natural) Octave.four
    ]


all : Test
all =
    describe "lines"
        [ test "C Minor pentatonic line within octave 4 should be C4, Eb4, F4, G4, Bb4" <|
            \_ ->
                Expect.equal (fromScaleWithinRange middleOctaveRange (Scale (PitchClass C Natural) minorPentatonic))
                    ([ PitchClass C Natural, PitchClass E Flat, PitchClass F Natural, PitchClass G Natural, PitchClass B Flat ] |> List.map (\note -> Pitch note Octave.four))
        , test "apply formula -2, -1, 1, -2 to C minor pentatonic from Bb3 - C6 starting with C" <|
            \_ ->
                Expect.equal (Line.applyFormula (PitchClass C Natural) formula line) expected
        ]
