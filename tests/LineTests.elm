module LineTests exposing (..)

import Test exposing (..)
import Expect
import Types.Scale exposing (..)
import Types.Note exposing (..)
import Types.Line as Line exposing (..)
import Types.Range exposing (..)
import Types.Octave as Octave exposing (..)
import Types.Pitch as Pitch exposing (..)
import Types.Formula as Formula exposing (..)


middleOctaveRange : Range
middleOctaveRange =
    OfPitch { lowest = Pitch (Note C Natural) Octave.four, highest = Pitch (Note B Natural) Octave.four }


line : Line
line =
    [ Pitch (Note B Flat) Octave.two
    , Pitch (Note C Natural) Octave.three
    , Pitch (Note E Flat) Octave.three
    , Pitch (Note F Natural) Octave.three
    , Pitch (Note G Natural) Octave.three
    , Pitch (Note B Flat) Octave.three
    , Pitch (Note C Natural) Octave.four
    , Pitch (Note E Flat) Octave.four
    , Pitch (Note F Natural) Octave.four
    , Pitch (Note G Natural) Octave.four
    , Pitch (Note B Flat) Octave.four
    ]


formula : Formula
formula =
    [ -2, 3, -1, 2 ]


expected : Line
expected =
    [ Pitch (Note C Natural) Octave.four
    , Pitch (Note G Natural) Octave.three
    , Pitch (Note E Flat) Octave.four
    , Pitch (Note C Natural) Octave.four
    , Pitch (Note F Natural) Octave.four
    , Pitch (Note C Natural) Octave.four
    , Pitch (Note G Natural) Octave.four
    , Pitch (Note F Natural) Octave.four
    , Pitch (Note B Flat) Octave.four
    ]


all : Test
all =
    describe "lines"
        [ test "C Minor pentatonic line within octave 4 should be C4, Eb4, F4, G4, Bb4" <|
            \_ ->
                Expect.equal (fromScaleWithinRange middleOctaveRange (Scale (Note C Natural) minorPentatonic))
                    ([ Note C Natural, Note E Flat, Note F Natural, Note G Natural, Note B Flat ] |> List.map (\note -> Pitch note Octave.four))
        , test "apply formula -2, 3, -1, 2 to C minor pentatonic from Bb2 - Bb4 starting with C" <|
            \_ ->
                Expect.equal (Line.applyFormula (Note C Natural) formula line) expected
        ]
