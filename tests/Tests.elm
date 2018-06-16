module Tests exposing (..)

import Test exposing (..)
import Expect
import Types exposing (..)
import MidiConversions exposing (toMidiNumber)
import Octave exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Pitch to MIDI number"
        [ test "A0 should be 21" <|
            \_ ->
                Expect.equal (octave 0 |> Maybe.map (\o -> toMidiNumber (Pitch A Nothing o))) (Just 21)
        , test "C8 should be 108" <|
            \_ ->
                Expect.equal (octave 8 |> Maybe.map (\o -> toMidiNumber (Pitch C Nothing o))) (Just 108)
        , test "C4 should be 60" <|
            \_ ->
                Expect.equal (octave 4 |> Maybe.map (\o -> toMidiNumber (Pitch C Nothing o))) (Just 60)
        , test "F#5 should be 78" <|
            \_ ->
                Expect.equal (octave 5 |> Maybe.map (\o -> toMidiNumber (Pitch F (Just Sharp) o))) (Just 78)
        , test "Gb5 should be 78" <|
            \_ ->
                Expect.equal (octave 5 |> Maybe.map (\o -> toMidiNumber (Pitch G (Just Flat) o))) (Just 78)
        ]
