module LineTests exposing (..)

import Test exposing (..)
import Expect
import Types.Scale exposing (..)
import Types.Note exposing (..)
import Types.Line exposing (..)
import Types.Range exposing (..)
import Types.Octave as Octave exposing (..)
import Types.Pitch as Pitch exposing (..)


middleOctaveRange : Range
middleOctaveRange =
    OfPitch { lowest = Pitch (Note C Natural) Octave.four, highest = Pitch (Note B Natural) Octave.four }


all : Test
all =
    describe "lines"
        [ test "C Minor pentatonic line within octave 4 should be C4, Eb4, F4, G4, Bb4" <|
            \_ ->
                Expect.equal (fromScalaWithinRange middleOctaveRange (Scale (Note C Natural) minorPentatonic))
                    ([ Note C Natural, Note E Flat, Note F Natural, Note G Natural, Note B Flat ] |> List.map (\note -> Pitch note Octave.four))
        ]
