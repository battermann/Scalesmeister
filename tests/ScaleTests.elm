module ScaleTests exposing (..)

import Test exposing (..)
import Expect
import Types.Scale exposing (..)
import Types.Note exposing (..)
import Types.Line exposing (..)
import Types.Range exposing (..)


all : Test
all =
    describe "notes"
        [ test "notes of C minor pentatonic scale should  be [C, Eb, F, G, Bb]" <|
            \_ ->
                Expect.equal (notes (Scale (PitchClass C Natural) minorPentatonic)) [ PitchClass C Natural, PitchClass E Flat, PitchClass F Natural, PitchClass G Natural, PitchClass B Flat ]
        , test "notes of Db minor pentatonic scale should  be [Db, Fb, Gb, Ab, Cb]" <|
            \_ ->
                Expect.equal (notes (Scale (PitchClass D Flat) minorPentatonic)) [ PitchClass D Flat, PitchClass F Flat, PitchClass G Flat, PitchClass A Flat, PitchClass C Flat ]
        , test "notes of C# minor pentatonic scale should  be [C#, E, F#, G#, B]" <|
            \_ ->
                Expect.equal (notes (Scale (PitchClass C Sharp) minorPentatonic)) [ PitchClass C Sharp, PitchClass E Natural, PitchClass F Sharp, PitchClass G Sharp, PitchClass B Natural ]
        , test "notes of B minor pentatonic scale should  be [B, D, E, F#, A]" <|
            \_ ->
                Expect.equal (notes (Scale (PitchClass B Natural) minorPentatonic)) [ PitchClass B Natural, PitchClass D Natural, PitchClass E Natural, PitchClass F Sharp, PitchClass A Natural ]
        ]
