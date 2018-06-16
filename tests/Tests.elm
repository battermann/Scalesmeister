module Tests exposing (..)

import Test exposing (..)
import Expect
import Types exposing (..)
import MidiConversions exposing (toMidiNumber)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Pitch to MIDI number"
        [ test "A0 should be 21" <|
            \_ ->
                Expect.equal (toMidiNumber (Pitch A Nothing 0)) 21
        , test "C8 should be 108" <|
            \_ ->
                Expect.equal (toMidiNumber (Pitch C Nothing 8)) 108
        , test "C4 should be 60" <|
            \_ ->
                Expect.equal (toMidiNumber (Pitch C Nothing 4)) 60
        , test "F#5 should be 78" <|
            \_ ->
                Expect.equal (toMidiNumber (Pitch F (Just Sharp) 5)) 78
        ]
