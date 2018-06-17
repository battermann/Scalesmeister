module MidiConversionsTests exposing (..)

import Test exposing (..)
import Expect
import Types.Note exposing (..)
import Types.Pitch exposing (..)
import MidiConversions exposing (toMidiNumber)
import Types.Octave exposing (..)


all : Test
all =
    describe "Pitch to MIDI number"
        [ test "A0 should be 21" <|
            \_ ->
                Expect.equal (octave 0 |> Maybe.map (\o -> toMidiNumber (Pitch (Note A Natural) o))) (Just 21)
        , test "C8 should be 108" <|
            \_ ->
                Expect.equal (octave 8 |> Maybe.map (\o -> toMidiNumber (Pitch (Note C Natural) o))) (Just 108)
        , test "C4 should be 60" <|
            \_ ->
                Expect.equal (octave 4 |> Maybe.map (\o -> toMidiNumber (Pitch (Note C Natural) o))) (Just 60)
        , test "F#5 should be 78" <|
            \_ ->
                Expect.equal (octave 5 |> Maybe.map (\o -> toMidiNumber (Pitch (Note F Sharp) o))) (Just 78)
        , test "Gb5 should be 78" <|
            \_ ->
                Expect.equal (octave 5 |> Maybe.map (\o -> toMidiNumber (Pitch (Note G Flat) o))) (Just 78)
        ]
