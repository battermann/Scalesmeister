module LineTests exposing (..)

import Test exposing (..)
import Expect
import Types.Scale exposing (..)
import Types.Note exposing (..)
import Types.Line as Line exposing (..)
import Types.Range as Range exposing (..)
import Types.Octave as Octave exposing (..)
import Types.Pitch as Pitch exposing (..)
import Types.Formula as Formula exposing (..)


middleOctaveRange : Range
middleOctaveRange =
    Range.piano
        |> Range.setLowest (Pitch (Note C Natural) Octave.four)
        |> Range.setHighest (Pitch (Note B Natural) Octave.four)


line : Line
line =
    [ Pitch (Note B Flat) Octave.three
    , Pitch (Note C Natural) Octave.four
    , Pitch (Note E Flat) Octave.four
    , Pitch (Note F Natural) Octave.four
    , Pitch (Note G Natural) Octave.four
    , Pitch (Note B Flat) Octave.four
    , Pitch (Note C Natural) Octave.five
    , Pitch (Note E Flat) Octave.five
    , Pitch (Note F Natural) Octave.five
    , Pitch (Note G Natural) Octave.five
    , Pitch (Note B Flat) Octave.five
    , Pitch (Note C Natural) Octave.six
    ]


formula : Formula
formula =
    [ -2, -1, 2, -1 ]


expected : Line
expected =
    [ Pitch (Note C Natural) Octave.six
    , Pitch (Note G Natural) Octave.five
    , Pitch (Note F Natural) Octave.five
    , Pitch (Note B Flat) Octave.five
    , Pitch (Note G Natural) Octave.five
    , Pitch (Note E Flat) Octave.five
    , Pitch (Note C Natural) Octave.five
    , Pitch (Note F Natural) Octave.five
    , Pitch (Note E Flat) Octave.five
    , Pitch (Note B Flat) Octave.four
    , Pitch (Note G Natural) Octave.four
    , Pitch (Note C Natural) Octave.five
    , Pitch (Note B Flat) Octave.four
    , Pitch (Note F Natural) Octave.four
    , Pitch (Note E Flat) Octave.four
    , Pitch (Note G Natural) Octave.four
    , Pitch (Note F Natural) Octave.four
    , Pitch (Note C Natural) Octave.four
    , Pitch (Note B Flat) Octave.three
    , Pitch (Note E Flat) Octave.four
    , Pitch (Note C Natural) Octave.four
    ]


all : Test
all =
    describe "lines"
        [ test "C Minor pentatonic line within octave 4 should be C4, Eb4, F4, G4, Bb4" <|
            \_ ->
                Expect.equal (fromScaleWithinRange middleOctaveRange (Scale (Note C Natural) minorPentatonic))
                    ([ Note C Natural, Note E Flat, Note F Natural, Note G Natural, Note B Flat ] |> List.map (\note -> Pitch note Octave.four))
        , test "apply formula -2, -1, 1, -2 to C minor pentatonic from Bb3 - C6 starting with C" <|
            \_ ->
                Expect.equal (Line.applyFormula (Note C Natural) formula line) expected
        ]
