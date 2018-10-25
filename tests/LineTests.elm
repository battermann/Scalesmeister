module LineTests exposing (all, expected, formula, line, middleOctaveRange)

import Expect
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (..)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (..)
import Types.Formula as Formula exposing (..)
import Types.Line as Line exposing (..)
import Types.Octave as Octave exposing (..)
import Types.Pitch as Pitch exposing (Pitch(..))
import Types.Range as Range exposing (..)


middleOctaveRange : Range
middleOctaveRange =
    Range.piano
        |> Range.setLowest (Pitch (pitchClass C natural) Octave.four)
        |> Range.setHighest (Pitch (pitchClass B natural) Octave.four)


line : Line
line =
    [ Pitch (pitchClass B flat) Octave.three
    , Pitch (pitchClass C natural) Octave.four
    , Pitch (pitchClass E flat) Octave.four
    , Pitch (pitchClass F natural) Octave.four
    , Pitch (pitchClass G natural) Octave.four
    , Pitch (pitchClass B flat) Octave.four
    , Pitch (pitchClass C natural) Octave.five
    , Pitch (pitchClass E flat) Octave.five
    , Pitch (pitchClass F natural) Octave.five
    , Pitch (pitchClass G natural) Octave.five
    , Pitch (pitchClass B flat) Octave.five
    , Pitch (pitchClass C natural) Octave.six
    ]


formula : Formula
formula =
    [ -2, -1, 2, -1 ]


expected : Line
expected =
    [ Pitch (pitchClass C natural) Octave.six
    , Pitch (pitchClass G natural) Octave.five
    , Pitch (pitchClass F natural) Octave.five
    , Pitch (pitchClass B flat) Octave.five
    , Pitch (pitchClass G natural) Octave.five
    , Pitch (pitchClass E flat) Octave.five
    , Pitch (pitchClass C natural) Octave.five
    , Pitch (pitchClass F natural) Octave.five
    , Pitch (pitchClass E flat) Octave.five
    , Pitch (pitchClass B flat) Octave.four
    , Pitch (pitchClass G natural) Octave.four
    , Pitch (pitchClass C natural) Octave.five
    , Pitch (pitchClass B flat) Octave.four
    , Pitch (pitchClass F natural) Octave.four
    , Pitch (pitchClass E flat) Octave.four
    , Pitch (pitchClass G natural) Octave.four
    , Pitch (pitchClass F natural) Octave.four
    , Pitch (pitchClass C natural) Octave.four
    , Pitch (pitchClass B flat) Octave.three
    , Pitch (pitchClass E flat) Octave.four
    , Pitch (pitchClass C natural) Octave.four
    ]


all : Test
all =
    describe "lines"
        [ test "C Minor pentatonic line within octave 4 should be C4, Eb4, F4, G4, Bb4" <|
            \_ ->
                Expect.equal (fromScaleWithinRange middleOctaveRange (Scale.scale (pitchClass C natural) ScaleClass.minorPentatonic))
                    ([ pitchClass C natural
                     , pitchClass E flat
                     , pitchClass F natural
                     , pitchClass G natural
                     , pitchClass B flat
                     ]
                        |> List.map (\note -> Pitch note Octave.four)
                    )
        , test "apply formula -2, -1, 1, -2 to C minor pentatonic from Bb3 - C6 starting with C" <|
            \_ ->
                Expect.equal (Line.applyFormula (pitchClass C natural) formula line) expected
        ]
