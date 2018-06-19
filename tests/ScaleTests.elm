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
                Expect.equal (notes (Scale (Note C Natural) minorPentatonic)) [ Note C Natural, Note E Flat, Note F Natural, Note G Natural, Note B Flat ]
        , test "notes of Db minor pentatonic scale should  be [Db, Fb, Gb, Ab, Cb]" <|
            \_ ->
                Expect.equal (notes (Scale (Note D Flat) minorPentatonic)) [ Note D Flat, Note F Flat, Note G Flat, Note A Flat, Note C Flat ]
        , test "notes of C# minor pentatonic scale should  be [C#, E, F#, G#, B]" <|
            \_ ->
                Expect.equal (notes (Scale (Note C Sharp) minorPentatonic)) [ Note C Sharp, Note E Natural, Note F Sharp, Note G Sharp, Note B Natural ]
        , test "notes of B minor pentatonic scale should  be [B, D, E, F#, A]" <|
            \_ ->
                Expect.equal (notes (Scale (Note B Natural) minorPentatonic)) [ Note B Natural, Note D Natural, Note E Natural, Note F Sharp, Note A Natural ]
        ]
