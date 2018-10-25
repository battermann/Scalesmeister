module LineTests exposing (all, expected, formula, line, middleOctaveRange)

import Expect
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (..)
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.PitchClass as PitchClass exposing (..)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (..)
import Types.Formula as Formula exposing (..)
import Types.Line as Line exposing (..)
import Types.Range as Range exposing (..)


middleOctaveRange : Range
middleOctaveRange =
    Range.piano
        |> Range.setLowest (Pitch.pitch C natural Octave.four)
        |> Range.setHighest (Pitch.pitch B natural Octave.four)


line : Line
line =
    [ Pitch.pitch B flat Octave.three
    , Pitch.pitch C natural Octave.four
    , Pitch.pitch E flat Octave.four
    , Pitch.pitch F natural Octave.four
    , Pitch.pitch G natural Octave.four
    , Pitch.pitch B flat Octave.four
    , Pitch.pitch C natural Octave.five
    , Pitch.pitch E flat Octave.five
    , Pitch.pitch F natural Octave.five
    , Pitch.pitch G natural Octave.five
    , Pitch.pitch B flat Octave.five
    , Pitch.pitch C natural Octave.six
    ]


formula : Formula
formula =
    [ -2, -1, 2, -1 ]


expected : Line
expected =
    [ Pitch.pitch C natural Octave.six
    , Pitch.pitch G natural Octave.five
    , Pitch.pitch F natural Octave.five
    , Pitch.pitch B flat Octave.five
    , Pitch.pitch G natural Octave.five
    , Pitch.pitch E flat Octave.five
    , Pitch.pitch C natural Octave.five
    , Pitch.pitch F natural Octave.five
    , Pitch.pitch E flat Octave.five
    , Pitch.pitch B flat Octave.four
    , Pitch.pitch G natural Octave.four
    , Pitch.pitch C natural Octave.five
    , Pitch.pitch B flat Octave.four
    , Pitch.pitch F natural Octave.four
    , Pitch.pitch E flat Octave.four
    , Pitch.pitch G natural Octave.four
    , Pitch.pitch F natural Octave.four
    , Pitch.pitch C natural Octave.four
    , Pitch.pitch B flat Octave.three
    , Pitch.pitch E flat Octave.four
    , Pitch.pitch C natural Octave.four
    ]


all : Test
all =
    describe "lines"
        [ test "C Minor pentatonic line within octave 4 should be C4, Eb4, F4, G4, Bb4" <|
            \_ ->
                Expect.equal (fromScaleWithinRange middleOctaveRange (Scale.scale (pitchClass C natural) ScaleClass.minorPentatonic))
                    [ Pitch.pitch C natural Octave.four
                    , Pitch.pitch E flat Octave.four
                    , Pitch.pitch F natural Octave.four
                    , Pitch.pitch G natural Octave.four
                    , Pitch.pitch B flat Octave.four
                    ]
        , test "apply formula -2, -1, 1, -2 to C minor pentatonic from Bb3 - C6 starting with C" <|
            \_ ->
                Expect.equal (Line.applyFormula (pitchClass C natural) formula line) expected
        ]
